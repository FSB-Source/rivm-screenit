package nl.rivm.screenit.service.colon;

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

import java.util.Set;

import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.colon.ColonUitnodiging;

import org.joda.time.DateTime;

public interface ColonUitnodigingService
{
	
	BriefDefinitie getBriefType(ColonUitnodiging colonUitnodiging);

	DateTime getGeprognotiseerdeIntakeDatum(boolean vooraankondigen);

	ColonUitnodiging cloneUitnodiging(ColonUitnodiging uitnodiging, boolean checkAlleenUitslagGecommuniceerd);

	Set<Integer> getAlleGeboortejarenTotMetHuidigJaar();

	void berekenEnSetUitgesteldeUitslagDatum(ColonUitnodiging uitnodiging);

	void verwijderUitgesteldeUitslagDatum(ColonUitnodiging uitnodiging);
}
