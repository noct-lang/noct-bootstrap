#include "module.hpp"
#include "common/name-mangling.hpp"

namespace Noctis
{
	ModuleEncodeInfo::ModuleEncodeInfo()
	{
		names.push_back("");
		nameIdMapping.try_emplace("", 1);
	}

	const StdString& ModuleEncodeInfo::GetNameFromId(u32 id)
	{
		assert(id <= names.size());
		return names[id];
	}	

	u32 ModuleEncodeInfo::GetOrAddName(const StdString& name)
	{
		auto it = nameIdMapping.find(name);
		if (it != nameIdMapping.end())
			return it->second;

		u32 id = u32(nameIdMapping.size());
		nameIdMapping.try_emplace(name, id);
		names.push_back(name);
		return id;
	}

	u32 ModuleEncodeInfo::GetNameId(const StdString& name)
	{
		auto it = nameIdMapping.find(name);
		if (it != nameIdMapping.end())
			return it->second;
		return 0;
	}

	QualNameSPtr ModuleEncodeInfo::GetQualNameFromId(Context* pCtx, u32 id, BoundsInfo* pBoundsInfo)
	{
		assert(id < names.size());
		const StdString& name = names[id];

		auto it = toQualNameMapping.find(name);
		if (it != toQualNameMapping.end())
		{
			StdPair<QualNameSPtr, BoundsInfo> pair = it->second;
			if (pBoundsInfo)
				pBoundsInfo->Merge(pair.second);
			return pair.first;
		}

		QualNameSPtr qualName = NameMangling::DemangleQualName(pCtx, name, pBoundsInfo);
		fromQualNameMapping.try_emplace(qualName, name);
		toQualNameMapping.try_emplace(name, std::pair{ qualName, pBoundsInfo  ? *pBoundsInfo : BoundsInfo{} });
		return qualName;
	}

	u32 ModuleEncodeInfo::GetOrAddQualName(Context* pCtx, QualNameSPtr qualName, BoundsInfo* pBoundsInfo)
	{
		StdString name;
		auto it = fromQualNameMapping.find(qualName);
		if (it != fromQualNameMapping.end())
		{
			name = it->second;
		}
		else
		{
			name = NameMangling::Mangle(pCtx, qualName, pBoundsInfo);
			fromQualNameMapping.try_emplace(qualName, name);
			toQualNameMapping.try_emplace(name, std::pair{ qualName, *pBoundsInfo });
		}
		return GetOrAddName(name);
	}

	u32 ModuleEncodeInfo::GetQualNameId(QualNameSPtr qualName)
	{
		auto it = fromQualNameMapping.find(qualName);
		if (it != fromQualNameMapping.end())
			return nameIdMapping[it->second];
		return 0;
	}

	TypeHandle ModuleEncodeInfo::GetTypeFromId(Context* pCtx, u32 id)
	{
		assert(id < names.size());
		const StdString& name = names[id];

		auto it = toTypeMapping.find(name);
		if (it != toTypeMapping.end())
			return it->second;

		TypeHandle type = NameMangling::DemangleType(pCtx, name);
		fromTypeMapping.try_emplace(type, name);
		toTypeMapping.try_emplace(name, type);
		return type;
	}

	u32 ModuleEncodeInfo::GetOrAddType(Context* pCtx, TypeHandle handle)
	{
		StdString name;
		auto it = fromTypeMapping.find(handle);
		if (it != fromTypeMapping.end())
		{
			name = it->second;
		}
		else
		{
			name = NameMangling::Mangle(pCtx, handle);
			fromTypeMapping.try_emplace(handle, name);
			toTypeMapping.try_emplace(name, handle);
		}
		return GetOrAddName(name);
	}

	Module::Module(QualNameSPtr qualName, Context* pCtx)
		: symTable(pCtx, qualName)
		, qualName(qualName)
		, opTable(pCtx)
		, isDecoded(false)
		, isImported(false)
		, dependencyGraph(pCtx)
	{
		memset(&header.fields, 0, sizeof(ModuleHeaderFields));
	}
}
