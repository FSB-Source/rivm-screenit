package nl.rivm.screenit.batch.jobs.cervix.cismigranten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.time.format.DateTimeFormatter;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
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

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixCISMigrantenWriter extends BaseWriter<Client>
{
	private final CervixFactory factory;

	private final LogService logService;

	private final SimplePreferenceService preferenceService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier dateSupplier;

	@Override
	protected void write(Client client) throws Exception
	{
		var startdatumBMHKString = preferenceService.getString(PreferenceKey.STARTDATUM_BMHK.name());
		var eindCISdatum = LocalDate.parse(startdatumBMHKString, DateTimeFormatter.ofPattern("yyyyMMdd"));

		var geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
		var huidigeLeeftijd = DateUtil.getLeeftijd(geboortedatum, dateSupplier.getLocalDate());

		if (huidigeLeeftijd < CervixLeeftijdcategorie.minimumLeeftijd() || huidigeLeeftijd >= CervixLeeftijdcategorie._65.getLeeftijd())
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client,
				"De client heeft een leeftijd die buiten de leeftijdsgrens van 30 en 65 valt om een nieuwe ronde te krijgen. leeftijd=" + huidigeLeeftijd);
			return;
		}

		var dossier = client.getCervixDossier();
		if (DossierStatus.INACTIEF.equals(dossier.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CISMIGRANTEN_UITNODIGEN_FOUT, client, "De cliënt is definitief afgemeld en ontvangt geen uitnodiging.");
			return;
		}

		var rondeBeginDatum = geboortedatum.plusYears(CervixLeeftijdcategorie.getLeeftijdcategorie(geboortedatum, dateSupplier.getLocalDateTime()).getLeeftijd());
		var ronde = dossier.getLaatsteScreeningRonde();
		var cisHistorie = getCISHistorie(client);

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

		if (ronde == null && rondeBeginDatum.isBefore(eindCISdatum))
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
		var cervixDossier = client.getCervixDossier();
		var cervixCisHistorie = cervixDossier.getCisHistorie();
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
