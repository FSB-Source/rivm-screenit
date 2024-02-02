package nl.rivm.screenit.model.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;

public class VerslagProjectVersionMapping
{

	private static final VerslagProjectVersionMapping instance = new VerslagProjectVersionMapping();

	private final Map<String, Map<VerslagType, VerslagGeneratie>> mapping = new HashMap<>();

	private VerslagProjectVersionMapping()
	{

		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2013-08-12T11:48:06", VerslagGeneratie.V1, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2014-04-25T12:07:09", VerslagGeneratie.V2, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2014-09-02T17:51:48", VerslagGeneratie.V3, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2015-06-17T10:32:08", VerslagGeneratie.V4, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2016-10-13T14:27:40", VerslagGeneratie.V5, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2017-11-16T22:49:21", VerslagGeneratie.V6, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2018-12-19T11:16:40", VerslagGeneratie.V7, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2019-11-17T20:43:52", VerslagGeneratie.V8, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2021-12-07T13:10:09", VerslagGeneratie.V10, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2023-01-12T11:59:05", VerslagGeneratie.V11, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2.16.840.1.113883.2.4.3.36.77.0.1.2022-09-13T14:46:06", VerslagGeneratie.V2, VerslagType.MAMMA_PA_FOLLOW_UP);
	}

	public void addProjectVersion(String projectVersion, VerslagGeneratie generatie, VerslagType... types)
	{
		Map<VerslagType, VerslagGeneratie> projectMapping = new EnumMap<>(VerslagType.class);
		if (types.length == 0)
		{

			List<String> teRemove = new ArrayList<>();
			mapping.forEach((key, value) ->
			{
				if (value.containsValue(generatie))
				{
					teRemove.add(key);
				}
			});
			teRemove.forEach(mapping::remove);
		}
		else
		{
			for (VerslagType type : types)
			{
				projectMapping.put(type, generatie);
			}
			mapping.put(projectVersion, projectMapping);
		}
	}

	public VerslagGeneratie getGeneratie(String projectVersion, VerslagType verslagType)
	{
		return mapping.get(projectVersion).get(verslagType);
	}

	public static VerslagProjectVersionMapping get()
	{
		return instance;
	}

	public String getFirstProjectVersion(VerslagGeneratie generatie, VerslagType verslagType)
	{
		for (Entry<String, Map<VerslagType, VerslagGeneratie>> e : mapping.entrySet())
		{
			if (generatie.equals(e.getValue().get(verslagType)))
			{
				return e.getKey();
			}
		}
		return "";
	}

	public VerslagGeneratie getHighestGeneratie(VerslagType verslagType)
	{
		VerslagGeneratie highestGeneratie = null;
		for (Entry<String, Map<VerslagType, VerslagGeneratie>> e : mapping.entrySet())
		{
			VerslagGeneratie verslagGeneratie = e.getValue().get(verslagType);
			if (verslagGeneratie != null && (highestGeneratie == null || verslagGeneratie.ordinal() > highestGeneratie.ordinal()))
			{
				highestGeneratie = verslagGeneratie;
			}
		}
		return highestGeneratie;
	}

}
