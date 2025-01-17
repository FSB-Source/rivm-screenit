package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.versturenstep;

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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.CervixHuisartsberichtenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixEdiService;
import nl.rivm.screenit.util.cervix.CervixLocatieUtil;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixHuisartsberichtVersturenWriter extends BaseWriter<CervixHuisartsBericht>
{

	private final LogService logService;

	private final CervixEdiService ediService;

	@Override
	protected void write(CervixHuisartsBericht huisartsBericht) throws Exception
	{
		try
		{
			if (CervixLocatieUtil.klantnummerNietGeverifieerd(huisartsBericht.getHuisartsLocatie()))
			{
				huisartsBericht.setStatus(CervixHuisartsBerichtStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
				getHibernateService().saveOrUpdate(huisartsBericht);
				List<Instelling> dashboardOrganisaties = rivmInstelling();
				dashboardOrganisaties.add(huisartsBericht.getScreeningsOrganisatie());
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSBERICHT_VERZENDEN_MISLUKT, dashboardOrganisaties, null, huisartsBericht.getClient(),
					String.format(
						"Het Zorgmail klantnummer van huisartslocatie: %s is niet geverifieerd. Bericht wordt alsnog gestuurd zodra het klantnummer door de huisarts is geverifieerd.",
						huisartsBericht.getHuisartsLocatie().getNaam()),
					Bevolkingsonderzoek.CERVIX);
			}
			else
			{
				LOG.info("Verstuur huisartsbericht:" + huisartsBericht.getId());
				ediService.verstuurMedVry(huisartsBericht, null);
			}
		}
		catch (IllegalStateException e)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSBERICHT_VERZENDEN_MISLUKT, (Account) null, huisartsBericht.getClient(), e.getMessage(),
				Bevolkingsonderzoek.CERVIX);
			LOG.error(e.getMessage(), e);
		}

		switch (huisartsBericht.getStatus())
		{
		case VERSTUURD:
			aantallenContextOphogen(CervixHuisartsberichtenConstants.VERSTUURD, CervixHuisartsberichtenConstants.BERICHT_TYPE_VERSTUURD,
				huisartsBericht.getBerichtType());
			break;
		case VERSTUREN_MISLUKT:
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSBERICHT_VERZENDEN_MISLUKT, (Account) null, huisartsBericht.getClient(), Bevolkingsonderzoek.CERVIX);
			aantallenContextOphogen(CervixHuisartsberichtenConstants.VERSTUREN_MISLUKT, CervixHuisartsberichtenConstants.BERICHT_TYPE_VERSTUREN_MISLUKT,
				huisartsBericht.getBerichtType());
			break;
		}
	}

	private List<Instelling> rivmInstelling()
	{
		List<Instelling> instellingen = new ArrayList<>();
		instellingen.addAll(getHibernateService().loadAll(Rivm.class));
		return instellingen;
	}
}
