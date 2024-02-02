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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;

public interface ColonTestService
{
	void importColonClientenViaCsv(File file, Map<String, ColonUitnodigingCategorie> categoriePerPatient, int startRondeCorrectie) throws IOException, ParseException;

	ColonBrief maakBrief(Client client, ColonScreeningRonde screeningRonde, BriefType briefType);

	ColonIntakeAfspraak maakAfspraak(GbaPersoon persoon, Date fitVerwerkingsDatum);

	ColonIntakeAfspraak maakAfspraak(GbaPersoon persoon, boolean eenmalig, Date fitVerwerkingsDatum);

	ColonConclusie maakAfspraakEnConclusie(GbaPersoon persoon, Date fitVerwerkingsDatum);

	void maakClientKlaarVoorRappeleren(GbaPersoon persoon);

	IFOBTTest maakHuidigeIFobtOntvangenInclUitslag(GbaPersoon filter, BigDecimal normwaarde, BigDecimal uitslag);

	void maakUitnodigingEnTestenVergelijkendOnderzoek(GbaPersoon filter);

	ColonScreeningRonde maakNieuweScreeningRonde(ColonDossier dossier);

	void huisartsBerichtKlaarzetten(GbaPersoon filter, HuisartsBerichtType berichtType);

	IFOBTTest maakHuidigeIFobtOntvangenEnGunstig(GbaPersoon persoon);

	IFOBTTest maakHuidigeIFobtOntvangenEnOngunstig(GbaPersoon persoon);

	void maakClientKlaarVoorAfronden(GbaPersoon modelObject);

	void huidigeIFOBTvoorRapelDatum(GbaPersoon modelObject);

	IFOBTTest zetVelorenIfobt(GbaPersoon modelObject, boolean zetUitslag, boolean gunstig);

	void brievenKlaarzetten(int aantal);

	String clientenResetten(String bsns);

	void clientReset(Client client);

	int markeerNogNietNaarInpakcentrumVerstuurdeUitnodigingenAlsVerstuurd();

	int verwijderRoosterBlokken();

	ColonUitnodigingCategorie getUitnodigingCategorie(Client client);
}
