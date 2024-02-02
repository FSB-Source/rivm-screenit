package nl.rivm.screenit.main.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.logging.LogRegel;

public interface ColonDossierService
{

	void monsterNietBeoordeelbaar(IFOBTTest ifobtTest);

	void conclusieOpslaan(ColonIntakeAfspraak afspraak, ColonVervolgonderzoekKeuzesDto keuzes, InstellingGebruiker ingelogdeGebruiker, ColonConclusieType voorgaandeConclusie);

	void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging, InstellingGebruiker ingelogdeGebruiker);

	boolean magConclusieAanpassenVerwijderen(ColonIntakeAfspraak afspraak, ColonConclusieType origConclusie);

	void conclusieVerwijderen(ColonIntakeAfspraak afspraak, InstellingGebruiker loggedInInstellingGebruiker, ColonConclusieType origConclusie);

	void verwijderIfobtUitslag(IFOBTTest buis, UploadDocument uploadDocument, InstellingGebruiker loggedInInstellingGebruiker);

	void vervangUitslagVerwijderenDocument(IFOBTTest buis, UploadDocument uploadDocument);

	boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, InstellingGebruiker medewerker, DashboardStatus dashboardStatus);
}
