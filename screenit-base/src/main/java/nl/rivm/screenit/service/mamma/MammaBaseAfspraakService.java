package nl.rivm.screenit.service.mamma;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MammaDagEnDagdeelFilter;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

public interface MammaBaseAfspraakService
{

	List<MammaKandidaatAfspraakDto> getKandidaatAfspraken(Client client, IMammaAfspraakWijzigenFilter filter);

	List<MammaKandidaatAfspraakDto> filterKandidaatAfsprakenOpDagEnDagdeel(List<MammaKandidaatAfspraakDto> afspraken, MammaDagEnDagdeelFilter filter);

	long countAfspraken(long standplaatsPeriodeId, MammaAfspraakStatus... afspraakStatussen);

	long countAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	int koppelNietGekoppeldeAfspraken(MammaCapaciteitBlok persistentBlok, boolean runDry);

	boolean valideUitstelStreefDatum(LocalDate streefDatum, MammaStandplaatsPeriode standplaatsPeriode);

	LocalDate vroegstMogelijkeUitnodigingsDatum(MammaDossier dossier, LocalDate voorstelDatum, Integer minimaleIntervalMammografieOnderzoeken);

	LocalDate laatstMogelijkeAfspraakDatum(MammaDossier dossier);

	List<MammaAfspraak> getAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	void bepaalBenodigdeCapaciteit(List<MammaAfspraak> afspraken, MammaScreeningsEenheid screeningsEenheid);

	MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean vorigeAfspraakVerzetten, boolean notificeerBetrokkenSe, boolean isBulk, boolean stuurBerichtNaarSectra, boolean logGebeurtenis,
		Account account, boolean isGeforceerdeAfspraak);

	MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean vorigeAfspraakVerzetten, boolean notificeerBetrokkenSe, boolean isBulk, boolean stuurBerichtNaarSectra, boolean logGebeurtenis,
		Account account, boolean isGeforceerdeAfspraak, SmsStatus smsStatus);

	BigDecimal getBenodigdeCapaciteit(List<MammaAfspraak> afspraken);

	void afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date rondeAfgemeldOp, boolean afspraakStatusWijzigen,
		boolean notificeerScreeningsEenhedenVerversenDaglijst);

	MammaStandplaatsLocatie getMammaStandplaatsLocatieAfspraak(MammaAfspraak afspraak);

	MammaStandplaatsLocatie getMammaStandplaatsLocatieUitnodiging(MammaUitnodiging uitnodiging);

	void afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date afgezegdOp);

	MammaAfspraak getLaatsteAfspraakVanBriefronde(Brief brief);

	MammaUitnodiging getLaatsteUitnodigingVanScreeningRonde(MammaScreeningRonde ronde);

	boolean isAfspraakBinnen180Dagen(MammaOnderzoek onderzoek);

	boolean isNoShow(MammaAfspraakStatus afspraakStatus, LocalDateTime afspraakMoment);

	boolean briefKanNogVerzondenWorden(Date afspraakDatum);

	boolean smsKanNogVerzondenWorden(LocalDateTime afspraakMoment);

	LocalDate getMinimaleAfspraakDatumBijUitnodigen();

	boolean magUitstellen(MammaDossier dossier);

	boolean magUitstellen(MammaDossier dossier, boolean bijAfspraakForceren);

}
