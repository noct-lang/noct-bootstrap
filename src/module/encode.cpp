#include "encode.hpp"

#include "common/name-mangling.hpp"
#include "common/context.hpp"
#include "common/errorsystem.hpp"
#include "common/logger.hpp"
#include "common/options.hpp"
#include "common/utils.hpp"
#include "il/il-encode.hpp"

namespace Noctis
{
	void ModuleSection::WriteTo(StdVector<u8>& buffer)
	{
		u8* headerAddr = reinterpret_cast<u8*>(&header);
		buffer.insert(buffer.end(), headerAddr, headerAddr + sizeof(ModuleSectionHeader));
		buffer.insert(buffer.end(), data.begin(), data.end());
	}

	ModuleEncode::ModuleEncode()
		: m_pMod(nullptr)
		, m_pBoundsInfo(nullptr)
	{
	}

	// TODO: mangle all names in name mangle pass to save overhead of mangling a lot here
	StdVector<u8> ModuleEncode::Encode(Module& mod)
	{
		m_pMod = &mod;
		
		// Encode all sections
		EncodeSymSection();
		EncodeInstSection();
		EncodeSLnkSection();
		EncodeILSection();

		EncodeNameSection();
		
		StdVector<u8> encoded;
		// Encode name
		QualNameSPtr packageQualName = mod.qualName->Base();
		StdString packageName;
		if (packageQualName)
		{
			packageName = packageQualName->ToString();
			StringReplace(packageName, "::", ".");
			encoded.insert(encoded.end(), packageName.begin(), packageName.end());
		}
		
		encoded.push_back(0);
		const StdString& modName = mod.qualName->LastIden();
		encoded.insert(encoded.end(), modName.begin(), modName.end());
		encoded.push_back(0);

		u32 importSize = EncodeImports(encoded);

		u32 idenSize = u32(packageName.length() + modName.length() + 2);
		u32 sectionCountOffset = u32(encoded.size());

		u8 numSections = 0;
		for (ModuleSection& section : m_Sections)
		{
			if (section.data.empty())
				continue;
			
			section.WriteTo(encoded);
			++numSections;
		}

		encoded.insert(encoded.begin() + sectionCountOffset, reinterpret_cast<u8*>(&numSections), reinterpret_cast<u8*>(&numSections) + sizeof(numSections));

		ModuleHeaderFields headerFields;
		
		headerFields.magic[0] = u8(0xFF);
		headerFields.magic[1] = 'N';
		headerFields.magic[2] = 'X';
		headerFields.magic[3] = 'M';
		headerFields.version = 0;
		headerFields.size = u64(encoded.size()) + sizeof(ModuleHeaderFields);
		headerFields.idenSize = idenSize;
		headerFields.importsSize = importSize;
		memset(headerFields.reserved, 0, sizeof(headerFields.reserved));
		encoded.insert(encoded.begin(), reinterpret_cast<u8*>(&headerFields), reinterpret_cast<u8*>(&headerFields) + sizeof(ModuleHeaderFields));

		return encoded;
	}

	u32 ModuleEncode::GetOrAddName(const StdString& name)
	{
		usize nameSize = m_pMod->encodeInfo.names.size();
		u32 id = m_pMod->encodeInfo.GetOrAddName(name);

		if (id >= nameSize)
		{
			m_NameSection.insert(m_NameSection.end(), name.begin(), name.end());
			m_NameSection.push_back(0);
		}

		return id;
	}

	u32 ModuleEncode::EncodeImports(StdVector<u8>& encoded)
	{
		if (m_pMod->imports.empty())
			return 0;

		u32 startPos = u32(encoded.size());
		for (StdPair<const QualNameSPtr, ModuleSPtr>& import : m_pMod->imports)
		{
			StdString modName = import.first->ToString();
			StringReplace(modName, "::", ".");

			// TODO: Flags
			WriteData(encoded, u8(0));
			encoded.insert(encoded.end(), modName.begin(), modName.end());
			encoded.push_back(0);
		}

		return u32(encoded.size()) - startPos;
	}

	void ModuleEncode::EncodeNameSection()
	{
		ModuleSection section;
		section.header.iden[0] = 'N';
		section.header.iden[1] = 'A';
		section.header.iden[2] = 'M';
		section.header.iden[3] = 'E';

		WriteData(section.data, u32(m_pMod->encodeInfo.names.size() - 1));
		for (usize i = 1; i < m_pMod->encodeInfo.names.size(); ++i)
		{
			const StdString& name = m_pMod->encodeInfo.names[i];
			section.data.insert(section.data.end(), name.begin(), name.end());
			WriteData(section.data, u8(0));
		}

		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);
		m_Sections.insert(m_Sections.begin(), section);
	}

	void ModuleEncode::EncodeSymSection()
	{
		ModuleSection section;
		section.header.iden[0] = 'S';
		section.header.iden[1] = 'Y';
		section.header.iden[2] = 'M';
		section.header.iden[3] = 'S';

		u32 numSyms = 0;
		
		m_pMod->symTable.Foreach([this, &section, &numSyms](SymbolSPtr sym, QualNameSPtr ifaceName)
		{
			if (sym->isImported)
				return;
			
			// Generic parameters are encoded in their parent symbols
			if (sym->kind == SymbolKind::GenVal)
				return;

			// Also skip types, since they are encoded in the child symbols
			if (sym->kind == SymbolKind::Type)
				return;

			m_pBoundsInfo = &sym->boundsInfo;

			u8 visAndKind = (u8(sym->vis) << 5) | u8(sym->kind);

			section.data.push_back(visAndKind);

			if (sym->kind == SymbolKind::Method ||
				sym->kind == SymbolKind::Typealias)
			{
				SymbolSPtr parent = sym->parent.lock();
				
				if (!sym->ifaces.empty())
				{
					// If we get here, we have a parent

					SymbolInstSPtr ifaceInst = sym->ifaces.front().lock();
					WriteData(section.data, u16(1 + ifaceInst->ifaces.size()));
					WriteQualName(section.data, ifaceInst->qualName);
					for (SymbolInstWPtr subIFace : ifaceInst->ifaces)
					{
						WriteQualName(section.data, subIFace.lock()->qualName);
					}
				}
				else
				{
					WriteData(section.data, u16(0));
				}

				if (sym->parent.lock())
					WriteType(section.data, sym->parent.lock()->SelfType());
				else
					WriteData(section.data, u8(0));
			}
			
			StdString mangleName;
			switch (sym->kind)
			{
			case SymbolKind::Type:
				mangleName = GetMangledType(sym->type);
				break;
			case SymbolKind::Method:
				mangleName = GetMangledQualName(QualName::Create(sym->qualName->LastIden()));
				break;
			case SymbolKind::Func:
			case SymbolKind::Closure:
			default:
				mangleName = GetMangledQualName(sym->qualName);
			}
			WriteName(section.data, mangleName);

			switch (sym->kind)
			{
			case SymbolKind::ValEnumMember:
			case SymbolKind::AdtEnumMember:
			{
				TypeHandle type = sym->type;
				if (!type.IsValid())
					type = g_TypeReg.Iden(TypeMod::None, sym->qualName->Base(usize(-1)));

				WriteType(section.data, type);
				break;
			}
			case SymbolKind::Typedef:
			case SymbolKind::Func:
			case SymbolKind::Method:
			case SymbolKind::Var:
			{
				WriteType(section.data, sym->type);
				break;
			}
			case SymbolKind::Typealias:
			{
				TypeSPtr type = sym->type.Type();
				if (type->typeKind != TypeKind::Iden ||
					type->AsIden().qualName != sym->qualName)
				{
					WriteType(section.data, sym->type);
				}
				else
				{
					WriteData(section.data, u8(0));
				}
				break;
			}
			default: ;
			}

			m_SymbolIdMapping.try_emplace(sym, numSyms);

			++numSyms;
		});

		if (numSyms == 0)
			return;

		u8* pNumSymsAddr = reinterpret_cast<u8*>(&numSyms);
		section.data.insert(section.data.begin(), pNumSymsAddr, pNumSymsAddr + sizeof(u32));

		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);
		m_Sections.push_back(section);
	}

	void ModuleEncode::EncodeInstSection()
	{
		ModuleSection section;
		section.header.iden[0] = 'I';
		section.header.iden[1] = 'N';
		section.header.iden[2] = 'S';
		section.header.iden[3] = 'T';

		u32 numInsts = 0;
		m_pMod->symTable.Foreach([this, &section, &numInsts](SymbolSPtr sym, QualNameSPtr)
		{
			if (sym->instantiations.empty())
				return;

			StdVector<SymbolInstSPtr> insts;
			for (SymbolInstSPtr inst : sym->instantiations)
			{
				if (!inst->isImported)
					insts.push_back(inst);
			}

			if (insts.empty())
				return;
			
			WriteData(section.data, u32(insts.size()));
			WriteQualName(section.data, sym->qualName);
			
			for (SymbolInstSPtr inst : insts)
			{
				WriteQualName(section.data, inst->qualName);
			}
			++numInsts;
		});

		if (numInsts == 0)
			return;

		u8* pNumInstsAddr = reinterpret_cast<u8*>(&numInsts);
		section.data.insert(section.data.begin(), pNumInstsAddr, pNumInstsAddr + sizeof(u32));

		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);
		m_Sections.push_back(section);
	}

	void ModuleEncode::EncodeSLnkSection()
	{
		ModuleSection section;
		section.header.iden[0] = 'S';
		section.header.iden[1] = 'L';
		section.header.iden[2] = 'N';
		section.header.iden[3] = 'K';

		u32 numLinks = 0;
		m_pMod->symTable.Foreach([this, &section, &numLinks](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (sym->isImported)
				return;
			
			if (sym->kind == SymbolKind::GenVal)
				return;
			
			if (!sym->ifaces.empty())
			{
				switch (sym->kind)
				{
				case SymbolKind::Struct:
				case SymbolKind::Union:
				case SymbolKind::ValEnum:
				case SymbolKind::AdtEnum:
				case SymbolKind::Typedef:
				case SymbolKind::StrongInterface:
				{
					WriteData(section.data, u8(SymbolLinkKind::InterfaceImpl));
					WriteData(section.data, u16(sym->ifaces.size()));
					WriteSymbolId(section.data, sym);
					
					StdVector<StdString> ifaceNames;
					for (SymbolInstWPtr iface : sym->ifaces)
					{
						WriteQualName(section.data, iface.lock()->qualName);
					}

					++numLinks;
					break;
				}
				case SymbolKind::Type:
				{
					StdVector<StdString> ifaceNames;
					for (SymbolInstWPtr iface : sym->ifaces)
					{
						if (iface.lock()->sym.lock()->isImported)
							continue;
						
						const StdString& iFaceMangle = GetMangledQualName(iface.lock()->qualName);
						ifaceNames.push_back(iFaceMangle);
					}

					if (ifaceNames.empty())
						break;

					WriteData(section.data, u8(SymbolLinkKind::TypeImpl));
					WriteData(section.data, u16(ifaceNames.size()));
					WriteType(section.data, sym->type);
					for (const StdString& name : ifaceNames)
						WriteName(section.data, name);
					++numLinks;
					
					break;
				}
				default:;
				}
			}

			if (!sym->orderedVarChildren.empty())
			{
				section.data.push_back(u8(SymbolLinkKind::MemberOrder));
				WriteData(section.data, u16(sym->orderedVarChildren.size()));
				WriteSymbolId(section.data, sym);

				for (SymbolWPtr child : sym->orderedVarChildren)
				{
					WriteSymbolId(section.data, child.lock());
				}

				++numLinks;
			}
		});

		if (numLinks == 0)
			return;

		u8* numLinksAddr = reinterpret_cast<u8*>(&numLinks);
		section.data.insert(section.data.begin(), numLinksAddr, numLinksAddr + sizeof(u32));
		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);
		m_Sections.push_back(section);
	}

	void ModuleEncode::EncodeILSection()
	{
		if (!g_Ctx.options.GetBuildOptions().encodeIL ||
			m_pMod->ilMod.funcs.empty())
			return;

		ModuleSection section;
		section.header.iden[0] = 'I';
		section.header.iden[1] = 'L';
		section.header.iden[2] = 'B';
		section.header.iden[3] = 'C';

		ILEncode ilEncode;
		section.data = ilEncode.Encode(m_pMod->ilMod);
		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);

		m_Sections.push_back(section);
	}

	void ModuleEncode::WriteName(StdVector<u8>& insertTo, const StdString& name)
	{
		u32 id = GetOrAddName(name);
		WriteId(insertTo, id);
	}

	void ModuleEncode::WriteSymbolId(StdVector<u8>& insertTo, SymbolSPtr sym)
	{
		u32 id = m_SymbolIdMapping.at(sym);
		WriteId(insertTo, id);
	}

	void ModuleEncode::WriteId(StdVector<u8>& insertTo, u32 id)
	{
		// Encode id
		bool moreThan1Byte = id > 0x7F;
		u8 byte0 = (moreThan1Byte << 7) | id & 0x7F;
		WriteData(insertTo, byte0);

		id >>= 7;
		if (!id)
			return;

		bool moreThan2Bytes = id > 0x7F;
		u8 byte1 = (moreThan2Bytes << 7) | id & 0x7F;
		WriteData(insertTo, byte1);

		id >>= 7;
		if (!id)
			return;

		bool moreThan3Bytes = id > 0x7F;
		u8 byte2 = (moreThan3Bytes << 7) | id & 0x7F;
		WriteData(insertTo, byte2);

		id >>= 7;
		if (!id)
			return;

		u8 byte3 = id & 0xFF;
		WriteData(insertTo, byte3);

		if (id > 0xFF)
		{
			g_ErrorSystem.Error("Cannot encode an id bigger than 29 bytes");
		}
	}

	void ModuleEncode::WriteType(StdVector<u8>& insertTo, TypeHandle type)
	{
		const StdString& mangled = GetMangledType(type);
		WriteName(insertTo, mangled);
	}

	void ModuleEncode::WriteQualName(StdVector<u8>& insertTo, QualNameSPtr qualName)
	{
		const StdString& mangled = GetMangledQualName(qualName);
		WriteName(insertTo, mangled);
	}

	const StdString& ModuleEncode::GetMangledQualName(QualNameSPtr qualName)
	{
		u32 id = m_pMod->encodeInfo.GetOrAddQualName(qualName, m_pBoundsInfo);
		return m_pMod->encodeInfo.GetNameFromId(id);
	}
	const StdString& ModuleEncode::GetMangledType(TypeHandle type)
	{
		u32 id = m_pMod->encodeInfo.GetOrAddType(type);
		return m_pMod->encodeInfo.GetNameFromId(id);
	}

	ModuleDecode::ModuleDecode()
		: m_pMod(nullptr)
		, m_pData(nullptr)
		, m_DataPos(0)
		, m_pBoundsInfo(nullptr)
	{
	}

	ModuleSPtr ModuleDecode::CreateModuleWithHeader(const StdString& filePath)
	{
		std::ifstream fin{ filePath.c_str(), std::ios::binary };
		if (!fin.is_open())
			return ModuleSPtr{};

		ModuleHeaderFields headerFields;
		fin.read(reinterpret_cast<char*>(&headerFields), sizeof(ModuleHeaderFields));

		// Check magic
		if (headerFields.magic[0] != char(0xFF) ||
			headerFields.magic[1] != 'N' ||
			headerFields.magic[2] != 'X' ||
			headerFields.magic[3] != 'M')
		{
			fin.close();
			g_ErrorSystem.Error("Trying to decode nxm with invalid signature!\n");
			return ModuleSPtr{};
		}

		// Check version
		if (headerFields.version != 0)
		{
			fin.close();
			g_ErrorSystem.Error("Trying to decode nxm with invalid version!\n");
			return ModuleSPtr{};
		}

		// Check reserved
		if (headerFields.reserved[0] != 0 ||
			headerFields.reserved[1] != 0 ||
			headerFields.reserved[2] != 0)
		{
			g_ErrorSystem.Error("nxm has reserved bytes set!\n");
		}

		StdVector<u8> idenData;
		idenData.resize(headerFields.idenSize);
		fin.read(reinterpret_cast<char*>(idenData.data()), headerFields.idenSize);

		StdVector<char> tmp;
		tmp.resize(headerFields.importsSize);
		fin.read(reinterpret_cast<char*>(tmp.data()), headerFields.importsSize);

		ModuleHeader header;
		header.fields = headerFields;
		fin.read(reinterpret_cast<char*>(&header.numSections), sizeof(header.numSections));

		// Check size
		fin.seekg(0, std::ios::end);
		u64 size = fin.tellg();
		fin.close();
		
		if (headerFields.size != size)
		{
			g_ErrorSystem.Error("nxm size does not match size in header!\n");
			return ModuleSPtr{};
		}

		m_DataPos = 0;
		StdString packageName = ExtractNullTermString(idenData, m_DataPos);
		QualNameSPtr packageQualname;
		if (packageName != "")
		{
			StdVector<StdString> packageSubNames = SplitString(packageName, '.');
			packageQualname = QualName::Create(packageSubNames);
		}
		
		StdString moduleName = ExtractNullTermString(idenData, m_DataPos);
		QualNameSPtr moduleQualName = packageQualname->Append(moduleName);

		u32 i = 0;
		while (i < tmp.size())
		{
			// TODO: flags
			++i;
			
			StdString fullImport{ tmp.data() + i };
			i += u32(fullImport.size()) + 1;

			StdVector<StdString> importNames = SplitString(fullImport, '.');
			header.imports.push_back(QualName::Create(importNames));
		}

		ModuleSPtr mod{ new Module { moduleQualName } };
		mod->header = header;
		mod->filePath = filePath;
		mod->isImported = true;
		return mod;
	}

	void ModuleDecode::Decode(Module& mod)
	{
		if (mod.isDecoded)
			return;

		Reset();
		
		m_pMod = &mod;
		UpdateImports();
		
		std::ifstream fin{ mod.filePath.c_str(), std::ios::binary | std::ios::ate };
		if (!fin.is_open())
			return;
		
		u64 startOffset = sizeof(ModuleHeaderFields) + mod.header.fields.idenSize + mod.header.fields.importsSize + sizeof(mod.header.numSections);
		u64 expectedSize = mod.header.fields.size - startOffset;
		u64 endSize = fin.tellg();
		fin.seekg(startOffset, std::ios::beg);
		u64 size = endSize - startOffset;

		// Check size
		if (expectedSize != size)
		{
			g_ErrorSystem.Error("nxm size does not match size in header!\n");
		}

		StdVector<u8> data;
		data.resize(size);
		fin.read(reinterpret_cast<char*>(data.data()), size);
		fin.close();

		m_pData = &data;
		m_DataPos = 0;

		while (m_DataPos < data.size())
		{
			const ModuleSectionHeader& header = *reinterpret_cast<const ModuleSectionHeader*>(&data[m_DataPos]);

			if (header.iden[0] == 'N' &&
				header.iden[1] == 'A' &&
				header.iden[2] == 'M' &&
				header.iden[3] == 'E')
			{
				DecodeName(header);
			}
			else if (header.iden[0] == 'S' &&
					 header.iden[1] == 'Y' &&
					 header.iden[2] == 'M' &&
					 header.iden[3] == 'S')
			{	
				DecodeSyms(header);
			}
			else if (header.iden[0] == 'I' &&
					 header.iden[1] == 'N' &&
					 header.iden[2] == 'S' &&
					 header.iden[3] == 'T')
			{
				DecodeInst(header);
			}
			else if (header.iden[0] == 'S' &&
					 header.iden[1] == 'L' &&
					 header.iden[2] == 'N' &&
					 header.iden[3] == 'K')
			{
				DecodeSLnk(header);
			}
			else if (header.iden[0] == 'I' &&
					 header.iden[1] == 'L' &&
					 header.iden[2] == 'B' &&
					 header.iden[3] == 'C')
			{
				DecodeILBC(header);
			}
			else
			{
				g_ErrorSystem.Error("Invalid section header: '%c%c%c%c', trying from next byte", header.iden[0], header.iden[1], header.iden[2], header.iden[3]);
				++m_DataPos;
			}
		}

		ParentSyms();
		
		for (StdPair<QualNameSPtr, SymbolSPtr> pair : m_Syms)
		{
			if (pair.second->parent.lock())
				continue;
			mod.symTable.Add(pair.second);
		}

		mod.isDecoded = true;

		data.clear();


		// Decode imported modules
		for (StdPair<const QualNameSPtr, ModuleSPtr>& import : mod.imports)
		{
			ModuleDecode subDecoder;
			subDecoder.Decode(*import.second);
		}
	}

	void ModuleDecode::Reset()
	{
		m_Syms.clear();
		m_ImplSyms.clear();
	}

	void ModuleDecode::UpdateImports()
	{
		for (QualNameSPtr import : m_pMod->header.imports)
		{
			ModuleSPtr mod = g_Ctx.modules.at(import);
			m_pMod->imports.try_emplace(import, mod);
		}
	}

	void ModuleDecode::DecodeName(const ModuleSectionHeader& header)
	{
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		u32 numNames = ReadData<u32>();

		for (u32 i = 0; i < numNames; ++i)
		{
			StdString name = ExtractNullTermString(data, m_DataPos);
			u32 id = m_pMod->encodeInfo.GetOrAddName(name);
		}
	}

	void ModuleDecode::DecodeSyms(const ModuleSectionHeader& header)
	{
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		u32 numSyms = ReadData<u32>();

		for (u32 i = 0; i < numSyms; ++i)
		{
			u8 visAndKind = ReadData<u8>();
			
			SymbolKind kind = static_cast<SymbolKind>(visAndKind & 0x1F);
			Visibility vis = static_cast<Visibility>(visAndKind >> 5);

			BoundsInfo boundsInfo;
			m_pBoundsInfo = &boundsInfo;

			SymbolSPtr sym;
			QualNameSPtr qualName;
			StdVector<QualNameSPtr> ifaceQualNames;
			if (kind == SymbolKind::Method ||
				kind == SymbolKind::Typealias)
			{
				u16 ifaceCount = ReadData<u16>();
				
				for (u16 j = 0; j < ifaceCount; ++j)
				{
					ifaceQualNames.push_back(ReadQualName());
				}

				TypeHandle parentType = ReadType();
				if (parentType.IsValid())
				{
					QualNameSPtr idenQualName = ReadQualName();

					if (parentType.Type()->typeKind == TypeKind::Iden)
					{
						qualName = parentType.AsIden().qualName;
						qualName = qualName->AppendLastIden(idenQualName);
					}
					else
					{
						StdString mangled = NameMangling::Mangle(parentType);
						qualName = QualName::Create(mangled)->AppendLastIden(idenQualName);
					}
				}
				else
				{
					qualName = ReadQualName();
				}
			}
			else
			{
				qualName = ReadQualName();
			}

			sym = CreateSymbol(kind, qualName);
			sym->isImported = true;
			sym->mangledName = NameMangling::Mangle(sym);
			sym->vis = vis;
			sym->boundsInfo = boundsInfo;
			m_pBoundsInfo = nullptr;

			// Handle type
			switch (kind)
			{
			case SymbolKind::Struct:
			case SymbolKind::Union:
			case SymbolKind::AdtEnum:
			case SymbolKind::MarkerInterface:
			case SymbolKind::WeakInterface:
			case SymbolKind::StrongInterface:
			{
				sym->type = g_TypeReg.Iden(TypeMod::None, qualName);
				g_TypeReg.SetIdenSym(sym->type.AsIden().qualName, sym);
				break;
			}
			case SymbolKind::ValEnum:
				// TODO
			case SymbolKind::ValEnumMember:
			case SymbolKind::AdtEnumMember:
			case SymbolKind::Typealias:
			case SymbolKind::Typedef:
			case SymbolKind::Func:
			case SymbolKind::Method:
			case SymbolKind::Var:
			{
				TypeHandle type = ReadType();
				if (kind != SymbolKind::Typealias || type.IsValid())
				{
					sym->type = type;
				}
				else
				{
					sym->type = g_TypeReg.Iden(TypeMod::None, sym->qualName);
					g_TypeReg.SetIdenSym(sym->type.AsIden().qualName, sym);
				}
				break;
			}
			default: ;
			}
			
			if (!ifaceQualNames.empty())
			{
				for (QualNameSPtr ifaceQualName : ifaceQualNames)
				{
					auto it = m_ImplSyms.find(ifaceQualName);
					if (it == m_ImplSyms.end())
						it = m_ImplSyms.try_emplace(ifaceQualName, StdUnorderedMap<QualNameSPtr, SymbolSPtr>{}).first;

					it->second.try_emplace(sym->qualName, sym);
				}
			}
			else
			{	
				m_Syms.try_emplace(sym->qualName, sym);
			}

			m_Insts.try_emplace(sym->qualName, sym->baseInst);
			m_SymIdMapping.push_back(sym);
		}

		if (m_DataPos != sectionEnd)
		{
			g_ErrorSystem.Error("nxm contains invalid 'SYMS' section!\n");
		}
	}

	void ModuleDecode::DecodeInst(const ModuleSectionHeader& header)
	{
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);
		
		u32 numEntries = ReadData<u32>();
		for (u32 i = 0; i < numEntries; ++i)
		{
			u32 numInsts = ReadData<u32>();
			QualNameSPtr ifaceQualName = ReadQualName();
			SymbolSPtr iface = GetSymbol(ifaceQualName);

			for (u32 j = 0; j < numInsts; ++j)
			{
				QualNameSPtr instQualName = ReadQualName();
				SymbolInstSPtr inst = iface->GetOrCreateInst(instQualName);
				inst->isImported = true;
				m_Insts.try_emplace(instQualName, inst);
			}
		}

		if (m_DataPos != sectionEnd)
		{
			g_ErrorSystem.Error("nxm contains invalid 'INST' section!\n");
		}
	}

	void ModuleDecode::DecodeSLnk(const ModuleSectionHeader& header)
	{
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		u32 numLinks = ReadData<u32>();
		for (u32 i = 0; i < numLinks; ++i)
		{
			SymbolLinkKind lnkKind = SymbolLinkKind(ReadData<u8>());

			switch (lnkKind)
			{
			case SymbolLinkKind::InterfaceImpl:
			{
				u16 numIfaces = ReadData<u16>();

				u32 symId = ReadId();
				SymbolSPtr sym = m_SymIdMapping[symId];

				for (u16 j = 0; j < numIfaces; ++j)
				{
					QualNameSPtr ifaceQualName = ReadQualName();
					SymbolInstSPtr inst = m_Insts.at(ifaceQualName);
					
					sym->ifaces.emplace_back(inst);
					inst->sym.lock()->impls.emplace_back(sym, inst);
				}
				
				break;
			}
			case SymbolLinkKind::TypeImpl:
			{
				u16 numIfaces = ReadData<u16>();
				TypeHandle handle = ReadType();

				SymbolSPtr sym = g_Ctx.activeModule->symTable.Find(handle);
				if (!sym)
				{
					StdString symName = NameMangling::Mangle(handle);
					QualNameSPtr qualName = QualName::Create(symName);
					sym = CreateSymbol(SymbolKind::Type, qualName);
					sym->type = handle;
					g_Ctx.activeModule->symTable.Add(sym);
				}

				for (u16 j = 0; j < numIfaces; ++j)
				{
					QualNameSPtr ifaceQualName = ReadQualName();
					SymbolInstSPtr inst = m_Insts.at(ifaceQualName);

					sym->ifaces.emplace_back(inst);
					inst->sym.lock()->impls.emplace_back(sym, inst);
				}

				break;
			}
			case SymbolLinkKind::MemberOrder:
			{
				u16 numMembers = ReadData<u16>();
				u32 symId = ReadId();
				SymbolSPtr parentSym = m_SymIdMapping[symId];

				for (u16 i = 0; i < numMembers; ++i)
				{
					u32 childSymId = ReadId();
					SymbolSPtr childSym = m_SymIdMapping[childSymId];
					parentSym->orderedVarChildren.emplace_back(childSym);
				}
				break;
			}
			default: ;
			}
		}

		if (m_DataPos != sectionEnd)
		{
			g_ErrorSystem.Error("nxm contains invalid 'SYMS' section!\n");
		}
	}

	void ModuleDecode::DecodeILBC(const ModuleSectionHeader& header)
	{
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		StdVector<u8> bytecode;
		bytecode.insert(bytecode.end(), data.begin() + m_DataPos, data.begin() + sectionEnd);
		m_DataPos = sectionEnd;

		ILDecode decode{ m_pMod };
		m_pMod->ilMod = decode.Decode(bytecode);
	}

	void ModuleDecode::ParentSyms()
	{
		for (StdPair<QualNameSPtr, SymbolSPtr> pair : m_Syms)
		{
			QualNameSPtr qualName = pair.first;
			if (qualName->Depth() == 1)
				continue;

			SymbolSPtr parent;
			usize numGenerics = pair.first->Generics().size();
			for (usize i = 0; i <= numGenerics; ++i)
			{
				QualNameSPtr possibleParent = qualName->Base(i);
				auto it = m_Syms.find(possibleParent);
				if (it != m_Syms.end())
				{
					parent = it->second;
					break;
				}
			}
			
			if (parent)
				parent->children->Add(pair.second);
		}

		for (StdPair<const QualNameSPtr, StdUnorderedMap<QualNameSPtr, SymbolSPtr>>& ifacePair : m_ImplSyms)
		{
			for (StdPair<QualNameSPtr, SymbolSPtr> pair : ifacePair.second)
			{
				SymbolSPtr parent;
				usize numGenerics = pair.first->Generics().size();
				for (usize i = 0; i <= numGenerics; ++i)
				{
					QualNameSPtr parentName = pair.first->Base(i);
					parent = GetSymbol(parentName);

					if (parent)
						break;
				}
				
				parent->children->Add(pair.second, ifacePair.first);
			}
		}
	}

	SymbolSPtr ModuleDecode::GetSymbol(QualNameSPtr qualName)
	{
		SymbolSPtr sym;
		// TODO: base generic
		auto it = m_Syms.find(qualName);
		if (it != m_Syms.end())
			sym = it->second;
		else
			sym = g_Ctx.activeModule->symTable.Find(qualName);

		if (!sym && qualName->Depth() == 1)
		{
			TypeHandle type = NameMangling::DemangleType(qualName->LastIden());
			sym = g_Ctx.activeModule->symTable.Find(type);

			if (!sym)
			{
				sym = CreateSymbol(SymbolKind::Type, qualName);
				sym->type = type;
				sym->mangledName = qualName->LastIden();
				g_Ctx.activeModule->symTable.Add(sym);
			}
		}

		return sym;
	}

	u32 ModuleDecode::ReadId()
	{
		u8 byte = ReadData<u8>();
		u32 id = byte & 0x7F;

		if (byte & 0x80)
		{
			byte = ReadData<u8>();
			id |= (byte & 0x7F) << 7;

			if (byte & 0x80)
			{
				byte = ReadData<u8>();
				id |= (byte & 0x7F) << 14;

				if (byte & 0x80)
				{
					byte = ReadData<u8>();
					id |= byte << 21;
				}
			}
		}

		return id;
	}

	QualNameSPtr ModuleDecode::ReadQualName()
	{
		u32 id = ReadId();
		return GetQualNameFromId(id);
	}

	TypeHandle ModuleDecode::ReadType()
	{
		u32 id = ReadId();
		return GetTypeFromId(id);
	}

	QualNameSPtr ModuleDecode::GetQualNameFromId(u32 id)
	{
		return m_pMod->encodeInfo.GetQualNameFromId(id, m_pBoundsInfo);
	}

	TypeHandle ModuleDecode::GetTypeFromId(u32 id)
	{
		if (id == 0)
			return TypeHandle{};
		
		return m_pMod->encodeInfo.GetTypeFromId(id);
	}
}
