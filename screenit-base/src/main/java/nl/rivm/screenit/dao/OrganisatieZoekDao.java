
package nl.rivm.screenit.dao;

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

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;

import org.hibernate.criterion.Criterion;

public interface OrganisatieZoekDao
{

	Iterator<Instelling> searchOrganisatie(Instelling searchObject, Map<OrganisatieType, List<Instelling>> typeSelection, List<OrganisatieType> excludeOrganisatieTypes, long first,
		long count, String sortProperty, boolean asc);

	long countOrganisatie(Instelling searchObject, Map<OrganisatieType, List<Instelling>> typeSelection, List<OrganisatieType> excludeOrganisatieTypes);

	Criterion addHierarchieCrit(Entry<OrganisatieType, List<Instelling>> type, Map<String, String> aliassen, String root);

	List<Instelling> zoekOrganisatieMetFqdn(String fqdn);

}
