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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.verslag.DSValue;

public interface BaseVerslagService
{
	void verwijderVerslag(Verslag<?, ?> verslag, InstellingGebruiker instellingGebruiker, boolean heropenRondeEnDossier);

	void heropenRondeEnDossier(Verslag<?, ?> verslag);

	String createLogMelding(Verslag<?, ?> verslag);

	boolean isElektronischPalgaVerslag(MammaFollowUpVerslag followUpVerslag);

	MdlVerslag getMdlVerslagMetTNummer(PaVerslagContent verslagContent);

	List<PaVerslag> getPaVerslagMetTNummer(MdlVerslagContent verslagContent);

	DSValue getDsValue(String code, String codeSystem, String valueSetName);

	DSValue getDsValue(String code, String codeSystem, String valueSetName, boolean ignoreCase);

	boolean heeftMdlVerslagenMetOnderzoekDatum(MdlVerslag verslag, Date onderzoekDatum);

	void setBerichtenOpnieuwVerwerken(List<Long> ids);
}
