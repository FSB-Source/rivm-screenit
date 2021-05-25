package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;

public interface MammaScreeningsEenheidService
{
	List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorBeoordelingsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden);

	List<MammaScreeningsEenheid> getActieveScreeningsEenheden();

	List<MammaScreeningsEenheid> zoekScreeningsEenheden(MammaScreeningsEenheid zoekObject, ScreeningOrganisatie regio, int first, int count, String sortProperty,
		boolean isAscending);

	long countScreeningsEenheden(MammaScreeningsEenheid screeningsEenheid, ScreeningOrganisatie regio);

	List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorBeoordelingsEenheid(Instelling instelling);

	String getScreeningsEenhedenNamen(BeoordelingsEenheid beoordelingsEenheid);

	String getGekoppeldeScreeningsEenhedenTekst(MammaStandplaats standplaats);

	boolean saveOrUpdateSE(MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker instellingGebruiker);

	void deleteMammograaf(MammaMammograaf mammograaf, MammaScreeningsEenheid screeningsEenheid);

	List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie);

	long getAantalActieveGekoppeldeOnderzoeken(MammaScreeningsEenheid screeningsEenheid);

	long getAantalNietAfgerondeGekoppeldeBeoordelingen(MammaScreeningsEenheid screeningsEenheid);

	String magWordenGeactiveerd(MammaScreeningsEenheid screeningsEenheid);

	String magWordenGeinactiveerd(MammaScreeningsEenheid screeningsEenheid);

	String getCsvString(Iterator<? extends MammaScreeningsEenheid> screeningsEenheidIterator);

	boolean ipAdressenHebbenZelfdeGemeenschappelijkeBlokken(MammaScreeningsEenheid screeningsEenheid);

	String valideerMinderValideAfspraakPeriodes(MammaScreeningsEenheid screeningsEenheid);
}
