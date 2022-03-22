package nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.selectie.CervixSelectieConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixSelectieWriter extends BaseWriter<Client>
{
	@Autowired
	private CervixFactory factory;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	protected void write(Client client) throws Exception
	{
		CervixDossier dossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();

		CervixLeeftijdcategorie leeftijdcategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()),
			dateSupplier.getLocalDateTime());
		int leeftijd = CervixLeeftijdcategorie.getLeeftijd(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()), dateSupplier.getLocalDateTime());

		if (leeftijd < CervixLeeftijdcategorie._30.getLeeftijd())
		{
			CervixScreeningRonde ronde = factory.maakRonde(dossier, false);
			factory.maakVooraankondiging(ronde);
		}
		else if (CervixLeeftijdcategorie._30.equals(leeftijdcategorie) && laatsteScreeningRonde != null)
		{
			factory.updateDossierMetVolgendeRondeDatum(dossier, dateSupplier.getLocalDateTime());
			factory.maakUitnodiging(laatsteScreeningRonde, laatsteScreeningRonde.getLeeftijdcategorie().getUitnodigingsBrief(), true, false);
		}
		else
		{
			CervixScreeningRonde ronde = factory.maakRonde(dossier);
			factory.maakUitnodiging(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief(), true, false);
		}

		aantalContextOphogen(CervixSelectieConstants.SELECTIE_AANTAL_KEY);
	}
}
