package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.time.LocalDate;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

public interface MammaBaseKansberekeningService
{
	void updateScreeningRondeEvent(MammaDossier dossier, boolean zetDeelname);

	void updateScreeningRondeEvent(MammaScreeningRonde screeningRonde, boolean zetDeelname);

	void updateAfspraakEvent(MammaAfspraak afspraak, boolean zetOpkomst);

	void fitDossierClassifier();

	void fitAfsprakenClassifier();

	void predictDossiers();

	void predictAfspraken();

	BigDecimal getOpkomstkans(MammaAfspraak afspraak);

	BigDecimal getVoorlopigeOpkomstkans(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, MammaVerzettenReden verzettenReden,
		BriefType briefTypeUitnodiging);

	BigDecimal getVoorlopigeOpkomstkans(MammaUitnodiging uitnodiging, MammaStandplaatsPeriode standplaatsPeriode, MammaVerzettenReden verzettenReden);

    void maakDossierEvent(MammaDossier dossier);

    void dossierEventHerzien(MammaDossier dossier);

	void screeningRondeSampleHerzien(MammaScreeningRonde screeningRonde);

	void kansberekeningHerzien(MammaDossier dossier, LocalDate vanaf);

	void resetPreferences();
}
