package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public abstract class CervixGevolgenLabprocesVerwerkenConstants
{

	public static final String AANTAL_IN_LABPROCES_KEY = "gevolgenLabprocesVerwerken.aantalInLabproces";

	public static final String AANTAL_DEFINITIEF_HERAANGEMELD_KEY = "gevolgenLabprocesVerwerken.aantalDefinitiefHeraangemeld";

	public static final String AANTAL_EENMALIG_HERAANGEMELD_KEY = "gevolgenLabprocesVerwerken.aantalEenmaligHeraangemeld";

	public static final String AANTAL_UITNODIGINGEN_UITSTRIJKJE_KEY = "gevolgenLabprocesVerwerken.aantalUitnodigingenUitstrijkje";

	public static final String AANTAL_UITNODIGINGEN_ZAS_KEY = "gevolgenLabprocesVerwerken.aantalUitnodigingenZas";

	public static final String AANTAL_IN_VERVOLGONDERZOEK_KEY = "gevolgenLabprocesVerwerken.aantalInVervolgonderzoek";

	public static final String AANTAL_RONDEN_GESLOTEN_KEY = "gevolgenLabprocesVerwerken.aantalRondenGesloten";

	public static final String TOTAAL_AANTAL_BRIEVEN_KEY = "gevolgenLabprocesVerwerken.totaalAantalBrieven";

	public static final String TOTAAL_AANTAL_HUISARTSBERICHTEN_KEY = "gevolgenLabprocesVerwerken.totaalAantalHuisartsberichten";

	public static final String BRIEF_TYPE_KEY = "gevolgenLabprocesVerwerken.briefType.";

	public static final String HUISARTSBERICHT_TYPE_KEY = "gevolgenLabprocesVerwerken.huisartsberichtType.";

}
