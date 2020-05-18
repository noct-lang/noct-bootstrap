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
	ModuleEncode::ModuleEncode(Context* pCtx)
		: m_pCtx(pCtx)
		, m_pMod(nullptr)
		, m_CurNameId(1)
	{
	}

	StdVector<u8> ModuleEncode::Encode(Module& mod)
	{
		m_pMod = &mod;
		
		// Encode all sections
		EncodeSymSection();
		EncodeSLnkSection();
		
		StdVector<u8> encoded;

		// Encode name
		QualNameSPtr packageQualName = mod.qualName->Base();
		StdString packageName;
		if (packageQualName)
		{
			packageName = packageQualName->ToString();
			packageName.erase(packageName.begin(), packageName.begin() + 2);
			StringReplace(packageName, "::", ".");
			encoded.insert(encoded.end(), packageName.begin(), packageName.end());
		}
		
		encoded.push_back(0);
		const StdString& modName = mod.qualName->Iden()->Name();
		encoded.insert(encoded.end(), modName.begin(), modName.end());
		encoded.push_back(0);

		u32 idenSize = u32(packageName.length() + modName.length() + 2);

		u32 sectionCountOffset = u32(encoded.size());
		u32 numSections = 0;

		{
			ModuleSectionHeader header;
			header.iden[0] = 'N';
			header.iden[1] = 'A';
			header.iden[2] = 'M';
			header.iden[3] = 'E';
			header.size = m_NameSection.size() + sizeof(ModuleSectionHeader) + sizeof(u32);

			WriteData(encoded, header);
			WriteData(encoded, m_CurNameId - 1); // - 1: because 0 is always ""
			encoded.insert(encoded.end(), m_NameSection.begin(), m_NameSection.end());
			++numSections;
		}

		{
			ModuleSectionHeader header;
			header.iden[0] = 'S';
			header.iden[1] = 'Y';
			header.iden[2] = 'M';
			header.iden[3] = 'S';
			header.size = m_SymSection.size() + sizeof(ModuleSectionHeader);

			WriteData(encoded, header);
			encoded.insert(encoded.end(), m_SymSection.begin(), m_SymSection.end());
			++numSections;
		}

		if (!m_SLnkSection.empty())
		{
			ModuleSectionHeader header;
			header.iden[0] = 'S';
			header.iden[1] = 'L';
			header.iden[2] = 'N';
			header.iden[3] = 'K';
			header.size = m_SLnkSection.size() + sizeof(ModuleSectionHeader);

			WriteData(encoded, header);
			encoded.insert(encoded.end(), m_SLnkSection.begin(), m_SLnkSection.end());
			++numSections;
		}

		if (m_pCtx->options.GetBuildOptions().encodeIL)
		{
			// TODO: Optional IL

			ILEncode ilEncode(m_pCtx);
			StdVector<u8> encodedIL = ilEncode.Encode(mod.ilMod);

			ModuleSectionHeader header;
			header.iden[0] = 'I';
			header.iden[1] = 'L';
			header.iden[2] = 'B';
			header.iden[3] = 'C';
			header.size = encodedIL.size() + sizeof(ModuleSectionHeader);

			encoded.insert(encoded.end(), reinterpret_cast<u8*>(&header), reinterpret_cast<u8*>(&header) + sizeof(ModuleSectionHeader));
			encoded.insert(encoded.end(), encodedIL.begin(), encodedIL.end());
			++numSections;
		}


		encoded.insert(encoded.begin() + sectionCountOffset, reinterpret_cast<u8*>(&numSections), reinterpret_cast<u8*>(&numSections) + sizeof(u32));

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
		auto it = m_NameMapping.find(name);
		if (it != m_NameMapping.end())
			return it->second;

		u32 nameId = m_CurNameId++;

		m_NameMapping.try_emplace(name, nameId);

		m_NameSection.insert(m_NameSection.end(), name.begin(), name.end());
		m_NameSection.push_back(0);

		return nameId;
	}

	void ModuleEncode::EncodeSymSection()
	{
		m_pMod->symTable.Foreach([this](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (sym->isImported)
				return;
			
			// Generic parameters are encoded in their parent symbols
			if (sym->kind == SymbolKind::GenType ||
				sym->kind == SymbolKind::GenVal ||
				sym->kind == SymbolKind::ImplType)
				return;

			m_SymSection.push_back(u8(sym->kind));

			if (sym->kind == SymbolKind::Method ||
				sym->kind == SymbolKind::Typealias)
			{
				if (!sym->impls.empty())
				{
					QualNameSPtr ifaceQualName = sym->interface.lock()->qualName;
					StdString mangledIFaceName = NameMangling::Mangle(m_pCtx, ifaceQualName);
					WriteName(m_SymSection, mangledIFaceName);
				}
				else
				{
					WriteData(m_SymSection, u8(0));
				}
			}
			
			StdString mangleName = NameMangling::Mangle(m_pCtx, sym->qualName);
			WriteName(m_SymSection, mangleName);

			switch (sym->kind)
			{
			case SymbolKind::ValEnumMember:
			case SymbolKind::AdtEnumMember:
			case SymbolKind::Typedef:
			case SymbolKind::Func:
			case SymbolKind::Method:
			case SymbolKind::Var:
			{
				StdString mangledType = NameMangling::Mangle(m_pCtx, sym->type);
				WriteName(m_SymSection, mangledType);
				break;
			}
			case SymbolKind::Typealias:
			{
				TypeSPtr type = m_pCtx->typeReg.GetType(sym->type);
				if (type->typeKind != TypeKind::Iden ||
					type->AsIden().qualName != sym->qualName)
				{
					StdString mangledType = NameMangling::Mangle(m_pCtx, sym->type);
					WriteName(m_SymSection, mangledType);
				}
				else
				{
					WriteData(m_SymSection, u8(0));
				}
				break;
			}
			default: ;
			}
		});
	}

	void ModuleEncode::EncodeSLnkSection()
	{
		m_pMod->symTable.Foreach([this](SymbolSPtr sym, QualNameSPtr iface)
		{
			if (sym->isImported)
				return;
			
			if (sym->kind == SymbolKind::GenType ||
				sym->kind == SymbolKind::GenVal)
				return;
			
			StdString mangle;
			if (sym->kind == SymbolKind::ImplType)
			{
				mangle = NameMangling::Mangle(m_pCtx, sym->type);
			}
			else
			{
				mangle = NameMangling::Mangle(m_pCtx, sym->qualName);
			}
			
			SymbolSPtr parent = sym->parent.lock();
			if (parent && parent->kind == SymbolKind::ImplType)
			{
				m_SLnkSection.push_back(u8(SymbolLinkKind::TypeParent));
				WriteName(m_SLnkSection, mangle);

				if (iface)
				{
					StdString ifaceMangle = NameMangling::Mangle(m_pCtx, iface);
					WriteName(m_SLnkSection, ifaceMangle);
				}
				else
				{
					WriteData(m_SymSection, u8(0));
				}
				
				StdString parentMangle = NameMangling::Mangle(m_pCtx, parent->type);
				WriteName(m_SLnkSection, parentMangle);
			}

			if (!sym->impls.empty())
			{
				u32 numImpls = 0;
				for (StdPair<SymbolSPtr, bool>& pair : sym->impls)
					numImpls += u32(!pair.second);

				if (numImpls)
				{
					switch (sym->kind)
					{
					case SymbolKind::Struct:
					case SymbolKind::Union:
					case SymbolKind::ValEnum:
					case SymbolKind::AdtEnum:
					case SymbolKind::Typedef:
					case SymbolKind::ImplType:
					{
						m_SLnkSection.push_back(u8(SymbolLinkKind::InterfaceImpl));
						u16 numIFaces = u16(sym->impls.size());
						WriteData(m_SLnkSection, numIFaces);

						WriteName(m_SLnkSection, mangle);

						for (StdPair<SymbolSPtr, bool>& pair : sym->impls)
						{
							if (pair.second)
								continue;

							StdString iFaceMangle = NameMangling::Mangle(m_pCtx, pair.first->qualName);
							WriteName(m_SLnkSection, iFaceMangle);
						}
						break;
					}
					default:;
					}
				}
			}

			if (!sym->variants.empty())
			{
				m_SLnkSection.push_back(u8(SymbolLinkKind::Variants));
				u16 numVariants = u16(sym->variants.size());
				m_SLnkSection.insert(m_SLnkSection.end(), reinterpret_cast<u8*>(&numVariants), reinterpret_cast<u8*>(&numVariants) + sizeof(u16));

				WriteName(m_SLnkSection, mangle);
				
				for (SymbolSPtr variant : sym->variants)
				{
					StdString variantMangle = NameMangling::Mangle(m_pCtx, variant->qualName);
					WriteName(m_SLnkSection, variantMangle);
				}
			}
		});
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
			g_ErrorSystem.Error("Cannot encode a name id bigger than 29 bytes");
		}
	}

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
		StdVector<StdString> packageSubNames = SplitString(packageName, '.');
		QualNameSPtr packageQualname = QualName::Create(packageSubNames);

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
		ParentSymbols();

		for (StdPair<QualNameSPtr, SymbolSPtr> pair : m_Syms)
		{
			StdString mangled = pair.second->mangledName;
			if (pair.second->parent.lock() ||
				!pair.second->IsBaseVariant())
				continue;
			mod.symTable.Add(pair.second);
		}
	}

	void ModuleDecode::DecodeName(const ModuleSectionHeader& header)
	{
		const StdVector<u8> data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		u32 numNames = ReadData<u32>();

		m_Names.push_back("");

		for (u32 i = 0; i < numNames; ++i)
		{
			StdString name = ExtractNullTermString(data, m_DataPos);
			m_Names.push_back(std::move(name));
		}
	}

	void ModuleDecode::DecodeSyms(const ModuleSectionHeader& header)
	{
		const StdVector<u8> data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		while (m_DataPos < sectionEnd)
		{
			SymbolKind kind = static_cast<SymbolKind>(data[m_DataPos++]);

			QualNameSPtr ifaceQualName;
			if (kind == SymbolKind::Method ||
				kind == SymbolKind::Typealias)
			{
				const StdString& ifaceMangled = ReadName();
				ifaceQualName = NameMangling::DemangleQualName(m_pCtx, ifaceMangled);
			}

			const StdString& mangled = ReadName();
			QualNameSPtr qualName = NameMangling::DemangleQualName(m_pCtx, mangled);

			SymbolSPtr sym{ new Symbol{ m_pCtx, kind, qualName } };
			sym->isImported = true;
			sym->self = sym;
			sym->baseVariant = sym;
			sym->mangledName = mangled;

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
				const StdString& mangledType = ReadName();

				TypeHandle type;
				if (kind != SymbolKind::Typealias || mangledType != "")
				{
					type = NameMangling::DemangleType(m_pCtx, mangledType);
				}
				else
				{
					type = m_pCtx->typeReg.Iden(TypeMod::None, sym->qualName);
				}
				sym->type = type;
				break;
			}
			default: ;
			}

			// Handle generics
			IdenSPtr iden = qualName->Iden();
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
							SymbolSPtr genSym{ new Symbol{ m_pCtx, SymbolKind::GenType, genQualName } };
							generic.type = genSym->type = m_pCtx->typeReg.Iden(TypeMod::None, genQualName);
							genSym->self = genSym;
							genSym->baseVariant = genSym;
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
							SymbolSPtr genSym{ new Symbol{ m_pCtx, SymbolKind::GenVal, genQualName } };
							genSym->type = generic.type;
							genSym->self = genSym;
							genSym->baseVariant = genSym;
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
		}

		if (m_DataPos != sectionEnd)
		{
			g_ErrorSystem.Error("nxm contains invalid 'SYMS' section!\n");
		}
	}

	void ModuleDecode::DecodeSLnk(const ModuleSectionHeader& header)
	{
		const StdVector<u8> data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		while (m_DataPos < sectionEnd)
		{
			SymbolLinkKind lnkKind = SymbolLinkKind(data[m_DataPos++]);

			switch (lnkKind)
			{
			case SymbolLinkKind::TypeParent:
			{
				const StdString& childName = ReadName();
				QualNameSPtr childQualName = NameMangling::DemangleQualName(m_pCtx, childName);

				const StdString& iFaceName = ReadName();
				QualNameSPtr iFaceQualName = NameMangling::DemangleQualName(m_pCtx, iFaceName);

				StdString parentName = ReadName();
				TypeHandle handle = NameMangling::DemangleType(m_pCtx, parentName);
				parentName = m_pCtx->typeReg.ToString(handle);
				QualNameSPtr parentQualName = QualName::Create(Iden::Create(parentName));
				
				auto it = m_Syms.find(parentQualName);
				if (it == m_Syms.end())
				{
					SymbolSPtr tmpSym{ new Symbol{ m_pCtx, SymbolKind::ImplType, nullptr } };
					tmpSym->type = handle;
					it = m_Syms.try_emplace(parentQualName, tmpSym).first;
				}
				SymbolSPtr parent = it->second;

				SymbolSPtr child;
				if (iFaceQualName)
				{
					auto implIt = m_ImplSyms.find(iFaceQualName);
					auto it = implIt->second.find(childQualName);
					child = it->second;
					parent->children->AddChild(iFaceQualName, child);
				}
				else
				{
					auto it = m_Syms.find(childQualName);
					child = it->second;
					parent->children->AddChild(child);
				}

				child->parent = parent;
				break;
			}
			case SymbolLinkKind::InterfaceImpl:
			{
				u16 numImpls = ReadData<u16>();

				StdString symMangled = ReadName();
				SymbolSPtr sym;
				if (isdigit(symMangled[0]))
				{
					QualNameSPtr qualName = NameMangling::DemangleQualName(m_pCtx, symMangled);
					auto it = m_Syms.find(qualName);
					sym = it->second;
				}
				else
				{
					TypeHandle handle = NameMangling::DemangleType(m_pCtx, symMangled);
					symMangled = m_pCtx->typeReg.ToString(handle);
					QualNameSPtr qualName = QualName::Create(Iden::Create(symMangled));

					auto it = m_Syms.find(qualName);
					if (it == m_Syms.end())
					{
						SymbolSPtr tmpSym{ new Symbol{ m_pCtx, SymbolKind::ImplType, nullptr } };
						tmpSym->type = handle;
						it = m_Syms.try_emplace(qualName, tmpSym).first;
					}
					sym = it->second;
				}

				for (u16 i = 0; i < numImpls; ++i)
				{
					const StdString& ifaceMangled = ReadName();
					QualNameSPtr ifaceQualName = NameMangling::DemangleQualName(m_pCtx, ifaceMangled);
					SymbolSPtr iface = m_Syms.at(ifaceQualName);

					sym->impls.emplace_back(iface, true);
					iface->impls.emplace_back(sym, true);
					if (SymbolSPtr baseIFace = iface->baseVariant.lock(); baseIFace)
						baseIFace->impls.emplace_back(sym, true);
				}
				
				break;
			}
			case SymbolLinkKind::Variants:
			{
				u16 numVariants = ReadData<u16>();
				const StdString& baseMangle = ReadName();
				QualNameSPtr baseQualName = NameMangling::DemangleQualName(m_pCtx, baseMangle);
				SymbolSPtr baseSym = m_Syms.at(baseQualName);

				for (u16 i = 0; i < numVariants; ++i)
				{
					const StdString& varMangle = ReadName();
					QualNameSPtr varQualName = NameMangling::DemangleQualName(m_pCtx, varMangle);
					SymbolSPtr varSym = m_Syms.at(varQualName);

					varSym->baseVariant = baseSym;
					baseSym->variants.push_back(varSym);

					if (varSym->kind == SymbolKind::MarkerInterface ||
						varSym->kind == SymbolKind::WeakInterface ||
						varSym->kind == SymbolKind::StrongInterface)
					{
						if (!varSym->impls.empty())
						{
							for (StdPair<SymbolSPtr, bool>& pair : varSym->impls)
							{
								baseSym->impls.push_back(pair);
							}
						}
					}
				}
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
		const StdVector<u8> data = *m_pData;
		u64 sectionEnd = m_DataPos + header.size;
		m_DataPos += sizeof(ModuleSectionHeader);

		StdVector<u8> bytecode;
		bytecode.insert(bytecode.end(), data.begin() + m_DataPos, data.begin() + sectionEnd);
		m_DataPos = sectionEnd;

		ILDecode decode(m_pCtx);
		m_pMod->ilMod = decode.Decode(bytecode);
	}


	void ModuleDecode::ParentSymbols()
	{
		for (StdPair<const QualNameSPtr, SymbolSPtr>& pair : m_Syms)
		{
			SymbolSPtr sym = pair.second;
			if (!sym->IsBaseVariant())
				continue;

			if (!sym->qualName)
				continue;

			QualNameSPtr possibleParentName = sym->qualName->Base();
			if (!possibleParentName)
				continue;

			auto it = m_Syms.find(possibleParentName);
			if (it != m_Syms.end())
			{
				SymbolSPtr parent = it->second;
				parent->children->AddChild(sym);
				sym->parent = parent;
			}
		}

		for (StdPair<const QualNameSPtr, StdUnorderedMap<QualNameSPtr, SymbolSPtr>>& implPair : m_ImplSyms)
		{
			QualNameSPtr ifaceQualName = implPair.first;
			for (StdPair<const QualNameSPtr, SymbolSPtr>& pair : implPair.second)
			{
				SymbolSPtr sym = pair.second;
				if (!sym->IsBaseVariant() ||
					sym->parent.lock())
					continue;

				QualNameSPtr possibleParentName = sym->qualName->Base();
				auto it = m_Syms.find(possibleParentName);
				if (it != m_Syms.end())
				{
					SymbolSPtr parent = it->second;
					parent->children->AddChild(ifaceQualName, sym);
					sym->parent = parent;
				}
			}
		}
	}

	const StdString& ModuleDecode::ReadName()
	{
		u32 id = 0;

		u8 byte = ReadData<u8>();
		id |= byte & 0x7F;

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

		return m_Names[id];
	}
}
