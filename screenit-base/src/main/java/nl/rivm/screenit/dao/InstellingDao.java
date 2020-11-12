
package nl.rivm.screenit.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import javax.annotation.Nonnull;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieBoomWrapper;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.AntedateerRange;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;

import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;

public interface InstellingDao
{

	Instelling getInstellingBy(String key, String value);

	List<Instelling> getInstellingByOrganisatieTypes(List<OrganisatieType> organisatieTypes);

	List<InstellingGebruiker> getActieveInstellingGebruikers(@Nonnull Gebruiker gebruiker);

	List getInstellingGebruikersVoorInloggen(@Nonnull Gebruiker medewerker);

	Criterion getActieveRolCriterion(String igRolAlias, String rolAlias);

	<T extends Instelling> List<T> getActieveInstellingen(Class<T> typeInstelling);

	<T extends Instelling> List<T> getActieveInstellingenBinnenRegio(Class<T> typeInstelling, ScreeningOrganisatie regio);

	List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenBinnenRegio(ScreeningOrganisatie regio);

	List<Instelling> getPathologieLabs(Instelling instelling);

	<T extends Instelling> List<T> getChildrenInstellingen(@Nonnull Instelling instelling, @Nonnull Class<T> typeInstelling);

	IFobtLaboratorium getIfobtLabByLabID(String labID);

	List<OrganisatieBoomWrapper> getCompleteOrganisatieBoom();

	List<Long> getLandelijkeInstellingIds(OrganisatieType type);

	Criteria getAllILAdressenZonderCoordinanten();

	ScreeningOrganisatie getScreeningOrganisatie(String regioCode);

	boolean isErEenOverlappendeAntedateerRange(AntedateerRange nieuweRange);
}
