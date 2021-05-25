package nl.rivm.screenit.batch.jobs.cervix.cismigranten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.curator.shaded.com.google.common.primitives.Ints;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixCISMigrantenWriter extends BaseWriter<Client>
{

	@Autowired
	private CervixFactory factory;

	@Autowired
	private LogService logService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	protected void write(Client client) throws Exception
	{
		String startdatumBMHKString = preferenceService.getString(PreferenceKey.STARTDATUM_BMHK.name());
		LocalDate eindCIS = LocalDate.parse(startdatumBMHKString, DateTimeFormatter.ofPattern("yyyyMMdd"));

		LocalDate geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
		int huidigeLeeftijd = Ints.checkedCast(ChronoUnit.YEARS.between(geboortedatum, dateSupplier.getLocalDate()));

		if (huidigeLeeftijd < CervixLeeftijdcategorie._30.getLeeftijd() || huidigeLeeftijd >= CervixLeeftijdcategorie._65.getLeeftijd())
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client,
				"De client heeft een leeftijd die buiten de leeftijdsgrens van 30 en 65 valt om een nieuwe ronde te krijgen. leeftijd=" + huidigeLeeftijd);
			return;
		}

		CervixDossier dossier = client.getCervixDossier();
		if (DossierStatus.INACTIEF.equals(dossier.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client, "De cliënt is definitief afgemeld en ontvangt geen uitnodiging.");
			return;
		}

		LocalDate rondeBeginDatum = geboortedatum.plusYears(CervixLeeftijdcategorie.getLeeftijdcategorie(geboortedatum, dateSupplier.getLocalDateTime()).getLeeftijd());
		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		CervixCISHistorie cisHistorie = getCISHistorie(client);

		if (ronde != null && ScreeningRondeStatus.AFGEROND.equals(ronde.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client,
				"De ronde van deze cliënt is afgerond waardoor de cliënt geen uitnodiging ontvangt.");
			return;
		}

		if (cisHistorie.isHeeftUitslagInRonde0())
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client,
				"Deze client heeft al een uitslag gekregen vanuit CIS voor ronde 0 en heeft daarom op dit moment geen recht op een nieuwe ronde.");
			return;
		}

		if (ronde == null && rondeBeginDatum.isBefore(eindCIS))
		{
			ronde = factory.maakRonde(client.getCervixDossier());

			cisHistorie.setScreeningRonde(ronde);
			hibernateService.saveOrUpdate(cisHistorie);
		}
		else if (ronde == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client,
				"De huidige datum valt niet binnen ronde 0 voor deze cliënt en zal door de reguliere selectie een uitnodiging ontvangen.");
			return;
		}

		factory.maakUitnodiging(client.getCervixDossier().getLaatsteScreeningRonde(), BriefType.CERVIX_UITNODIGING, true, false);
		aantalContextOphogen(CervixCISMigrantenConstants.MIGRANT_UITNODIGINGEN_AANTAL_KEY);
	}

	private CervixCISHistorie getCISHistorie(Client client)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixCISHistorie cervixCisHistorie = cervixDossier.getCisHistorie();
		if (cervixCisHistorie == null)
		{
			cervixCisHistorie = new CervixCISHistorie();
			cervixCisHistorie.setDossier(cervixDossier);
			cervixDossier.setCisHistorie(cervixCisHistorie);
			hibernateService.saveOrUpdate(cervixCisHistorie);
			hibernateService.saveOrUpdate(cervixDossier);
		}
		return cervixCisHistorie;
	}
}
