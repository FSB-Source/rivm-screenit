package nl.rivm.screenit.service.cervix;

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

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.BriefType;

import com.aspose.words.Document;

public interface CervixTestService
{
	
	CervixDossier geefDossier(GbaPersoon gbaPersoon);

	CervixScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon);

	CervixScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon, Date inVervolgonderzoekDatum);

	CervixUitnodiging maakUitnodiging(GbaPersoon gbaPersoon, BriefType briefType);

	CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon);

	CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, String monsterId);

	CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, CervixUitstrijkjeStatus uitstrijkjeStatus, BMHKLaboratorium laboratorium);

	CervixUitstrijkje geefUitstrijkje(GbaPersoon gbaPersoon, CervixUitstrijkjeStatus uitstrijkjeStatus, String monsterId, BMHKLaboratorium bmhkLaboratorium);

	CervixZas geefZas(GbaPersoon gbaPersoon, CervixZasStatus zasStatus, BMHKLaboratorium bmhkLaboratorium);

	CervixZas geefZas(GbaPersoon gbaPersoon, CervixZasStatus zasStatus, String monsterId, BMHKLaboratorium bmhkLaboratorium);

	CervixHpvUitslag geefHpvUitslag(GbaPersoon gbaPersoon, CervixHpvUitslag hpvUitslag, BMHKLaboratorium laboratorium);

	CervixUitstrijkje geefCytologieUitslag(GbaPersoon gbaPersoon, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium);

	CervixUitstrijkje geefVervolgonderzoekUitslag(GbaPersoon gbaPersoon, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium);

	CervixLabformulier geefLabformulier(GbaPersoon gbaPersoon, CervixLabformulierStatus labformulierStatus, BMHKLaboratorium laboratorium, CervixHuisartsLocatie huisartsLocatie);

	CervixCytologieOrder geefCytologieOrder(GbaPersoon gbaPersoon);

	CervixHuisartsLocatie geefHuisartsLocatie();

	void importCervixClientenViaCsv(File file) throws IOException, ParseException;

	Document geefBarcodeUitnodigingsIdTestPdf(CervixUitnodiging uitnodiging);

	String clientenResetten(String bsns);

	void clientReset(Client client);

	int clientenDefinitiefAfmelden(List<Client> clienten, CervixAfmeldingReden afmeldingReden);
}
