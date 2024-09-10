package nl.rivm.screenit.service.colon;

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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;

public interface ColonBaseFITService
{
	Optional<IFOBTTest> getFit(String barcode);

	void uitslagFitOntvangen(IFOBTTest buis);

	void setStatus(IFOBTTest buis, IFOBTTestStatus nieuweStatus);

	void heraanmelden(ColonScreeningRonde screeningRonde, LocalDateTime nu);

	void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging);

	void verwijderUitslag(IFOBTTest buis, UploadDocument uploadDocument);

	void markeerBuisAlsVerloren(ColonUitnodiging uitnodiging);

	void monsterNietBeoordeelbaar(IFOBTTest buis);

	void checkVervaldatumVerlopen(IFOBTTest buis);

	void bepaalEnSetHeraanmeldenTekstKey(IFOBTTest ifobtTest);

	void setTestenVerlorenIndienActief(IFOBTTest test);

	Client getAndereClientOpZelfdeAdresEnActieveFit(Client client, List<Long> uitgenodigdeClientIds);

	Optional<IFOBTBestand> getIfobtBestand(String bestandsnaam);

	Optional<IFOBTTest> getLaatsteFitMetMissendeUitslagVanDossier(ColonDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum);

	boolean isVerwijderdeBarcode(String barcode);
}
