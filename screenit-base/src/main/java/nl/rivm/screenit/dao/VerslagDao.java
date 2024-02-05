
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.verslag.DSValue;

public interface VerslagDao
{
	MdlVerslag getMdlVerslagMetTNummer(PaVerslagContent verslagContent);

	List<PaVerslag> getPaVerslagMetTNummer(MdlVerslagContent verslagContent);

	DSValue getDsValue(String code, String codeSystem, String valueSetName);

	DSValue getDsValue(String code, String codeSystem, String valueSetName, boolean ignoreCase);

	List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client);

	List<OntvangenCdaBericht> searchBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending);

	long countBerichten(BerichtZoekFilter filter);

	List<Object> getIdsEnBerichtTypen(BerichtZoekFilter filter);

	MdlVerslag getActueelsteMdlVerslag(ColonScreeningRonde screeningRonde);

	long getMdlVerslagenWithOnderzoekDatum(Verslag verslag, Date aanvangVerrichting);

	<V extends Verslag> List<V> zoekVerslagen(V zoekObject, int first, int count, String property, boolean ascending);

	<V extends Verslag> long countVerslagen(V zoekObject);

	void setBerichtenOpnieuwVerwerken(List<Long> ids);
}
