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

	ModuleEncode::ModuleEncode(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
	{
	}

	// TODO: mangle all names in name mangle pass to save overhead of mangling a lot here
	StdVector<u8> ModuleEncode::Encode(Module& mod)
	{
		m_pMod = &mod;
		
		// Encode all sections
		EncodeImportSection();
		EncodeSymSection();
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
		const StdString& modName = mod.qualName->LastIden()->Name();
		encoded.insert(encoded.end(), modName.begin(), modName.end());
		encoded.push_back(0);

		u32 idenSize = u32(packageName.length() + modName.length() + 2);
		u32 sectionCountOffset = u32(encoded.size());

		for (ModuleSection& section : m_Sections)
		{
			section.WriteTo(encoded);
		}

		u8 numSections = u8(m_Sections.size());
		encoded.insert(encoded.begin() + sectionCountOffset, reinterpret_cast<u8*>(&numSections), reinterpret_cast<u8*>(&numSections) + sizeof(numSections));

		ModuleHeader header;
		header.magic[0] = u8(0xFF);
		header.magic[1] = 'N';
		header.magic[2] = 'X';
		header.magic[3] = 'M';
		header.version = 0;
		header.size = u64(encoded.size()) + sizeof(ModuleHeader) - sizeof(header.numSections);
		header.idenSize = idenSize;
		memset(header.reserved, 0, sizeof(header.reserved));
		encoded.insert(encoded.begin(), reinterpret_cast<u8*>(&header), reinterpret_cast<u8*>(&header) + sizeof(ModuleHeader) - sizeof(header.numSections));

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

	void ModuleEncode::EncodeImportSection()
	{
		ModuleSection section;
		section.header.iden[0] = 'I';
		section.header.iden[1] = 'M';
		section.header.iden[2] = 'P';
		section.header.iden[3] = 'O';

		WriteData(section.data, u16(m_pMod->imports.size()));
		for (StdPair<const QualNameSPtr, ModuleSPtr>& import : m_pMod->imports)
		{
			StdString modName = import.first->ToString();
			StringReplace(modName, "::", ".");

			// TODO: Flags
			WriteData(section.data, u8(0));
			section.data.insert(section.data.end(), modName.begin(), modName.end());
			section.data.push_back(0);
		}

		if (section.data.empty())
			return;

		section.header.size = section.data.size() + sizeof(ModuleSection);
		m_Sections.push_back(section);
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
		
		m_pMod->symTable.Foreach([this, &section](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (sym->isImported)
				return;
			
			// Generic parameters are encoded in their parent symbols
			if (sym->kind == SymbolKind::GenType ||
				sym->kind == SymbolKind::GenVal)
				return;

			// Also skip types, since they are encoded in the child symbols
			if (sym->kind == SymbolKind::Type)
				return;

			section.data.push_back(u8(sym->kind));

			if (sym->qualName->LastIden()->Name() == "opGe")
				int br = 0;

			if (sym->kind == SymbolKind::Method ||
				sym->kind == SymbolKind::Typealias)
			{
				if (!sym->impls.empty())
				{
					QualNameSPtr ifaceQualName = sym->interfaces[0].first;
					const StdString& mangledIFaceName = GetMangledQualName(ifaceQualName);
					WriteName(section.data, mangledIFaceName);
				}
				else
				{
					WriteData(section.data, u8(0));
				}

				if (sym->parent.lock())
				{
					StdString mangledName = GetMangledType(sym->parent.lock()->SelfType());
					WriteName(section.data, mangledName);
				}
				else
				{
					WriteData(section.data, u8(0));
				}
			}
			
			StdString mangleName;
			switch (sym->kind)
			{
				break;
			case SymbolKind::Type:
				mangleName = GetMangledType(sym->type);
				break;
			case SymbolKind::Func:
			case SymbolKind::Method:
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
					type = m_pCtx->typeReg.Iden(TypeMod::None, sym->qualName->Base());

				const StdString& mangledType = GetMangledType(type);
				WriteName(section.data, mangledType);
				break;
			}
			case SymbolKind::Typedef:
			case SymbolKind::Func:
			case SymbolKind::Method:
			case SymbolKind::Var:
			{
				const StdString& mangledType = GetMangledType(sym->type);
				WriteName(section.data, mangledType);
				break;
			}
			case SymbolKind::Typealias:
			{
				TypeSPtr type = sym->type.Type();
				if (type->typeKind != TypeKind::Iden ||
					type->AsIden().qualName != sym->qualName)
				{
					const StdString& mangledType = GetMangledType(sym->type);
					WriteName(section.data, mangledType);
				}
				else
				{
					WriteData(section.data, u8(0));
				}
				break;
			}
			default: ;
			}
		});

		if (section.data.empty())
			return;

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
		
		m_pMod->symTable.Foreach([this, &section](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (sym->isImported)
				return;
			
			if (sym->kind == SymbolKind::GenType ||
				sym->kind == SymbolKind::GenVal)
				return;
			
			StdString mangle;
			if (sym->kind == SymbolKind::Type)
			{
				mangle = GetMangledType(sym->type);
			}
			else
			{
				mangle = GetMangledQualName(sym->qualName);
			}

			if (!sym->impls.empty())
			{
				u32 numImpls = 0;
				for (SymbolSPtr implSym : sym->impls)
					numImpls += u32(implSym->kind != SymbolKind::Type && implSym->qualName->IsSubnameOf(m_pCtx->activeModule->qualName));

				if (numImpls)
				{
					switch (sym->kind)
					{
					case SymbolKind::Struct:
					case SymbolKind::Union:
					case SymbolKind::ValEnum:
					case SymbolKind::AdtEnum:
					case SymbolKind::Typedef:
					case SymbolKind::Type:
					{
						section.data.push_back(u8(SymbolLinkKind::InterfaceImpl));
						u16 numIFaces = u16(sym->impls.size());
						WriteData(section.data, numIFaces);

						WriteName(section.data, mangle);

						for (SymbolSPtr implSym : sym->impls)
						{
							if (implSym->qualName->IsSubnameOf(m_pCtx->activeModule->qualName))
								continue;

							const StdString& iFaceMangle = GetMangledQualName(implSym->qualName);
							WriteName(section.data, iFaceMangle);
						}
						break;
					}
					default:;
					}
				}
			}

			if (!sym->variants.empty())
			{
				section.data.push_back(u8(SymbolLinkKind::Variants));
				WriteData(section.data, u16(sym->variants.size()));

				WriteName(section.data, mangle);
				
				for (SymbolSPtr variant : sym->variants)
				{
					const StdString& variantMangle = GetMangledQualName(variant->qualName);
					WriteName(section.data, variantMangle);
				}
			}

			if (!sym->orderedVarChildren.empty())
			{
				section.data.push_back(u8(SymbolLinkKind::MemberOrder));
				WriteData(section.data, u16(sym->orderedVarChildren.size()));
				WriteName(section.data, mangle);

				for (SymbolWPtr child : sym->orderedVarChildren)
				{
					QualNameSPtr qualName = child.lock()->qualName;
					const StdString& mangleChild = GetMangledQualName(qualName);
					WriteName(section.data, mangleChild);
				}
			}
		});

		if (section.data.empty())
			return;

		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);
		m_Sections.push_back(section);
	}

	void ModuleEncode::EncodeILSection()
	{
		if (!m_pCtx->options.GetBuildOptions().encodeIL ||
			m_pMod->ilMod.funcs.empty())
			return;

		ModuleSection section;
		section.header.iden[0] = 'I';
		section.header.iden[1] = 'L';
		section.header.iden[2] = 'B';
		section.header.iden[3] = 'C';

		ILEncode ilEncode(m_pCtx);
		section.data = ilEncode.Encode(m_pMod->ilMod);
		section.header.size = section.data.size() + sizeof(ModuleSectionHeader);

		m_Sections.push_back(section);
	}

	void ModuleEncode::WriteName(StdVector<u8>& insertTo, const StdString& name)
	{
		u32 id = GetOrAddName(name);

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
			g_ErrorSystem.Error("Cannot encode  a name id bigger than 29 bytes");
		}
	}

	const StdString& ModuleEncode::GetMangledQualName(QualNameSPtr qualName)
	{
		u32 id = m_pMod->encodeInfo.GetOrAddQualName(m_pCtx, qualName);
		return m_pMod->encodeInfo.GetNameFromId(id);
	}
	const StdString& ModuleEncode::GetMangledType(TypeHandle type)
	{
		u32 id = m_pMod->encodeInfo.GetOrAddType(m_pCtx, type);
		return m_pMod->encodeInfo.GetNameFromId(id);
	}

	// TODO: Cache demangled names
	ModuleDecode::ModuleDecode(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
		, m_pData(nullptr)
		, m_DataPos(0)
	{
	}

	ModuleSPtr ModuleDecode::CreateModuleWithHeader(const StdString& filePath)
	{
		std::ifstream fin{ filePath.c_str(), std::ios::binary };
		if (!fin.is_open())
			return ModuleSPtr{};

		ModuleHeader header;
		fin.read(reinterpret_cast<char*>(&header), sizeof(ModuleHeader) - sizeof(header.numSections));

		// Check magic
		if (header.magic[0] != char(0xFF) ||
			header.magic[1] != 'N' ||
			header.magic[2] != 'X' ||
			header.magic[3] != 'M')
		{
			fin.close();
			g_ErrorSystem.Error("Trying to decode nxm with invalid signature!\n");
			return ModuleSPtr{};
		}

		// Check version
		if (header.version != 0)
		{
			fin.close();
			g_ErrorSystem.Error("Trying to decode nxm with invalid version!\n");
			return ModuleSPtr{};
		}

		// Check reserved
		if (header.reserved[0] != 0 ||
			header.reserved[1] != 0 ||
			header.reserved[2] != 0)
		{
			g_ErrorSystem.Error("nxm has reserved bytes set!\n");
		}

		StdVector<u8> idenData;
		idenData.resize(header.idenSize);
		fin.read(reinterpret_cast<char*>(idenData.data()), header.idenSize);

		fin.read(reinterpret_cast<char*>(&header.numSections), sizeof(header.numSections));

		// Check size
		fin.seekg(0, std::ios::end);
		u64 size = fin.tellg();
		fin.close();
		
		if (header.size != size)
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
		QualNameSPtr moduleQualName = QualName::Create(packageQualname, moduleName);

		ModuleSPtr mod{ new Module { moduleQualName, m_pCtx } };
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
		
		std::ifstream fin{ mod.filePath.c_str(), std::ios::binary | std::ios::ate };
		if (!fin.is_open())
			return;
		
		u64 expectedSize = mod.header.size - mod.header.idenSize - sizeof(ModuleHeader);
		u64 startOffset = sizeof(ModuleHeader) + mod.header.idenSize;
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
		m_pMod = &mod;

		while (m_DataPos < data.size())
		{
			const ModuleSectionHeader& header = *reinterpret_cast<const ModuleSectionHeader*>(&data[m_DataPos]);

			if (header.iden[0] == 'I' &&
				header.iden[1] == 'M' &&
				header.iden[2] == 'P' &&
				header.iden[3] == 'O')
			{
				DecodeImport(header);
			}
			else if (header.iden[0] == 'N' &&
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
		}

		for (StdPair<QualNameSPtr, SymbolSPtr> pair : m_Syms)
		{
			StdString mangled = pair.second->mangledName;
			if (pair.second->parent.lock() ||
				!pair.second->IsBaseVariant())
				continue;
			mod.symTable.Add(pair.second);
		}

		mod.isDecoded = true;

		data.clear();

		// Decode imported modules
		for (StdPair<const QualNameSPtr, ModuleSPtr>& import : mod.imports)
		{
			ModuleDecode subDecoder{ m_pCtx };
			subDecoder.Decode(*import.second);
		}
	}

	void ModuleDecode::Reset()
	{
		m_Syms.clear();
		m_ImplSyms.clear();
	}

	void ModuleDecode::DecodeImport(const ModuleSectionHeader& header)
	{
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		u32 numImports = ReadData<u16>();

		for (u32 i = 0; i < numImports; ++i)
		{
			// TODO: flags
			(void)ReadData<u8>();

			StdString modName = ExtractNullTermString(data, m_DataPos);
			StdVector<StdString> modSubName = SplitString(modName, '.');
			QualNameSPtr modQualName = QualName::Create(modSubName);

			ModuleSPtr mod = m_pCtx->modules.at(modQualName);
			m_pMod->imports.try_emplace(modQualName, mod);
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
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		while (m_DataPos < sectionEnd)
		{
			if (m_DataPos > 30140)
				int br = 0;
			
			SymbolKind kind = static_cast<SymbolKind>(data[m_DataPos++]);

			SymbolSPtr sym;
			QualNameSPtr qualName;
			QualNameSPtr ifaceQualName;
			if (kind == SymbolKind::Method ||
				kind == SymbolKind::Typealias)
			{
				ifaceQualName = ReadQualName();

				TypeHandle parentType = ReadType();
				if (parentType.IsValid())
				{
					IdenSPtr iden = ReadQualName()->LastIden();

					if (parentType.Type()->typeKind == TypeKind::Iden)
					{
						qualName = parentType.AsIden().qualName;
						qualName = QualName::Create(qualName, iden);
					}
					else
					{
						TypeDisambiguationSPtr disambig = TypeDisambiguation::Create(parentType, ifaceQualName);
						qualName = QualName::Create(QualName::Create(disambig), iden);
					}
				}
				else
				{
					qualName = ReadQualName();
				}
				if (qualName->LastIden()->Name() == "opNe")
					int br = 0;
			}
			else
			{
				qualName = ReadQualName();
				if (qualName->LastIden()->Name() == "opNe")
					int br = 0;
			}

			sym = CreateSymbol(m_pCtx, kind, qualName);
			sym->isImported = true;
			sym->mangledName = NameMangling::Mangle(m_pCtx, sym);

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
				sym->type = m_pCtx->typeReg.Iden(TypeMod::None, qualName);
				m_pCtx->typeReg.SetIdenSym(sym->type.AsIden().qualName, sym);
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
					sym->type = m_pCtx->typeReg.Iden(TypeMod::None, sym->qualName);
					m_pCtx->typeReg.SetIdenSym(sym->type.AsIden().qualName, sym);
				}
				break;
			}
			default: ;
			}

			// Handle generics
			IdenSPtr iden = qualName->LastIden();
			if (!iden->Generics().empty())
			{
				for (IdenGeneric& generic : iden->Generics())
				{
					if (generic.isType)
					{
						if (generic.isSpecialized)
						{
							// Nothing to do here for now
						}
						else
						{
							QualNameSPtr genQualName = QualName::Create(qualName, generic.iden);
							SymbolSPtr genSym = CreateSymbol(m_pCtx, SymbolKind::GenType, genQualName);
							generic.type = genSym->type = m_pCtx->typeReg.Iden(TypeMod::None, genQualName);
							sym->children->AddChild(genSym);
						}
					}
					else
					{
						if (generic.isSpecialized)
						{
							// TODO
						}
						else
						{
							QualNameSPtr genQualName = QualName::Create(qualName, generic.iden);
							SymbolSPtr genSym = CreateSymbol(m_pCtx, SymbolKind::GenVal, genQualName);
							genSym->type = generic.type;
							sym->children->AddChild(genSym);
						}
					}
				}
			}

			if (ifaceQualName)
			{
				auto it = m_ImplSyms.find(ifaceQualName);
				if (it == m_ImplSyms.end())
					it = m_ImplSyms.try_emplace(ifaceQualName, StdUnorderedMap<QualNameSPtr, SymbolSPtr>{}).first;

				it->second.try_emplace(sym->qualName, sym);
			}
			else
			{	
				m_Syms.try_emplace(sym->qualName, sym);
			}

			// Get and add parent if one exists
			QualNameSPtr possibleParentName = sym->qualName->Base();
			if (possibleParentName)
			{
				auto parentIt = m_Syms.find(possibleParentName);
				if (parentIt != m_Syms.end())
				{
					SymbolSPtr parent = parentIt->second;
					parent->children->AddChild(sym, ifaceQualName);
					sym->parent = parent;
				}
			}
		}

		if (m_DataPos != sectionEnd)
		{
			g_ErrorSystem.Error("nxm contains invalid 'SYMS' section!\n");
		}
	}

	void ModuleDecode::DecodeSLnk(const ModuleSectionHeader& header)
	{
		const StdVector<u8>& data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		while (m_DataPos < sectionEnd)
		{
			SymbolLinkKind lnkKind = SymbolLinkKind(data[m_DataPos++]);

			switch (lnkKind)
			{
			case SymbolLinkKind::InterfaceImpl:
			{
				u16 numImpls = ReadData<u16>();

				u32 symNameId = ReadNameId();
				StdString symName = m_pMod->encodeInfo.GetNameFromId(symNameId);
				SymbolSPtr sym;
				if (isdigit(symName[0]))
				{
					QualNameSPtr qualName = GetQualNameFromId(symNameId);
					auto it = m_Syms.find(qualName);
					sym = it->second;
				}
				else
				{
					TypeHandle handle = GetTypeFromId(symNameId);
					symName = m_pCtx->typeReg.ToString(handle);
					QualNameSPtr qualName = QualName::Create(Iden::Create(symName));

					auto it = m_Syms.find(qualName);
					if (it == m_Syms.end())
					{
						SymbolSPtr tmpSym = CreateSymbol(m_pCtx, SymbolKind::Type, nullptr);
						tmpSym->type = handle;
						it = m_Syms.try_emplace(qualName, tmpSym).first;
					}
					sym = it->second;
				}

				for (u16 i = 0; i < numImpls; ++i)
				{
					QualNameSPtr ifaceQualName = ReadQualName();
					SymbolSPtr iface = m_Syms.at(ifaceQualName);

					sym->impls.emplace_back(iface);
					iface->impls.emplace_back(sym);
					if (SymbolSPtr baseIFace = iface->baseVariant.lock(); baseIFace)
						baseIFace->impls.emplace_back(sym);
				}
				
				break;
			}
			case SymbolLinkKind::Variants:
			{
				u16 numVariants = ReadData<u16>();
				QualNameSPtr baseQualName = ReadQualName();
				SymbolSPtr baseSym = m_Syms.at(baseQualName);

				for (u16 i = 0; i < numVariants; ++i)
				{
					QualNameSPtr varQualName = ReadQualName();
					SymbolSPtr varSym = m_Syms.at(varQualName);

					varSym->baseVariant = baseSym;
					baseSym->variants.push_back(varSym);

					if (varSym->kind == SymbolKind::MarkerInterface ||
						varSym->kind == SymbolKind::WeakInterface ||
						varSym->kind == SymbolKind::StrongInterface)
					{
						if (!varSym->impls.empty())
						{
							for (SymbolSPtr implSym : varSym->impls)
							{
								baseSym->impls.push_back(implSym);
							}
						}
					}
				}
				break;
			}
			case SymbolLinkKind::MemberOrder:
			{
				u16 numMembers = ReadData<u16>();
				QualNameSPtr parentQualName = ReadQualName();
				SymbolSPtr parentSym = m_Syms.at(parentQualName);

				for (u16 i = 0; i < numMembers; ++i)
				{
					QualNameSPtr childQualName = ReadQualName();
					SymbolSPtr childSym = m_Syms.at(childQualName);

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

		ILDecode decode(m_pCtx, m_pMod);
		m_pMod->ilMod = decode.Decode(bytecode);
	}

	u32 ModuleDecode::ReadNameId()
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
		u32 id = ReadNameId();
		return GetQualNameFromId(id);
	}

	TypeHandle ModuleDecode::ReadType()
	{
		u32 id = ReadNameId();
		return GetTypeFromId(id);
	}

	QualNameSPtr ModuleDecode::GetQualNameFromId(u32 id)
	{
		return m_pMod->encodeInfo.GetQualNameFromId(m_pCtx, id);
	}

	TypeHandle ModuleDecode::GetTypeFromId(u32 id)
	{
		if (id == 0)
			return TypeHandle{};
		
		return m_pMod->encodeInfo.GetTypeFromId(m_pCtx, id);
	}
}
