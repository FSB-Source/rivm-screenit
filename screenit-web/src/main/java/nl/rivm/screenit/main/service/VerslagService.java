package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
public interface VerslagService
{

	void saveOrAfronden(VerslagContent<?> verslagContent, FormulierResultaat resultaat, boolean afronden, InstellingGebruiker instellingGebruiker);

	boolean magAfronden(VerslagType verslagType, Client client);

	void preFillAntwoorden(Verslag verslag, FormulierResultaatImpl formulierResultaat, Gebruiker gebruiker);

	List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client);

	List<OntvangenCdaBericht> searchBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending);

	long countBerichten(BerichtZoekFilter filter);

	public <V extends Verslag<?, ?>> V heropenVerslag(V verslag, InstellingGebruiker instellingGebruiker);

	void herverwerkAlleBerichten(BerichtZoekFilter nullSafeGet);

	<V extends Verslag<?, ?>> List<V> zoekVerslagen(V zoekObject, int first, int count, String property, boolean ascending);

	<V extends Verslag<?, ?>> long countVerslagen(V zoekObject);

	void berichtenOpnieuwVerwerken(List<Long> ids, Bevolkingsonderzoek bvo);

	void berichtOpnieuwVerwerken(OntvangenCdaBericht ontvangenCdaBericht);
}
