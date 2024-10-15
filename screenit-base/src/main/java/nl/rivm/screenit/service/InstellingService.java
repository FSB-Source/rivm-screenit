package nl.rivm.screenit.service;

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

import java.util.List;

import javax.annotation.Nonnull;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;

import org.hibernate.Criteria;

public interface InstellingService
{

	List<CentraleEenheid> getMogelijkeCentraleEenheden(Instelling instelling);

	List<InstellingGebruiker> getActieveInstellingGebruikers(@Nonnull Gebruiker medewerker);

	List<ColonIntakelocatie> getActieveIntakelocaties();

	List<ColonIntakelocatie> getActieveIntakelocatiesBinneRegio(ScreeningOrganisatie regio);

	List<BeoordelingsEenheid> getActieveBeoordelingseenhedenBinnenRegio(ScreeningOrganisatie regio);

	List<CentraleEenheid> getActieveCentraleEenhedenBinnenRegio(ScreeningOrganisatie regio);

	List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties();

	void saveOrUpdateScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie, List<Gemeente> choices, InstellingGebruiker loggedInInstellingGebruiker);

	void saveOrUpdateSoPlanningBk(ScreeningOrganisatie screeningOrganisatie, InstellingGebruiker loggedInInstellingGebruiker);

	void saveOrUpdate(Instelling organisatie);

	void saveOrUpdateColoscopieCentrum(ColonIntakelocatie intakelocatie);

	<T extends Instelling> List<T> getActieveInstellingen(Class<T> typeInstelling);

	Instelling getInstellingBy(String key, String value);

	List<Instelling> getPathologieLabs(@Nonnull Instelling instelling);

	<T extends Instelling> List<T> getChildrenInstellingen(@Nonnull Instelling instelling, @Nonnull Class<T> typeInstelling);

	List<Instelling> getInstellingByOrganisatieTypes(List<OrganisatieType> organisatieTypes);

	List<Gebruiker> getActieveGebruikers(@Nonnull Instelling instelling);

	void saveDocumentForInstelling(UploadDocument uploadDocument, Instelling instelling);

	void deleteDocumentForInstelling(UploadDocument document, Instelling instelling);

	IFobtLaboratorium getIfobtLabByLabID(String labID);

	Criteria getAllILAdressenZonderCoordinanten();

	ScreeningOrganisatie getScreeningOrganisatie(String regioCode);

	ScreeningOrganisatie getScreeningOrganisatie(long screeningOrganisatieId);

}
