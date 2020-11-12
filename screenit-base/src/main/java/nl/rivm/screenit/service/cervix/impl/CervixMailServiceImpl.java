package nl.rivm.screenit.service.cervix.impl;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.AsyncMailer;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

@Service
public class CervixMailServiceImpl implements CervixMailService
{

	@Autowired
	private ApplicationContext appContext;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private LogService logService;

	@Autowired
	private MailService mailService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public void sendBMHKBezwaarLichaamsmateriaalMailAsync(CervixMonster monster)
	{
		long clientid = getClientIdVanMonster(monster);
		final String mailContent = getBaseMailContent(PreferenceKey.CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL, monster);
		final String subject = preferenceService.getString(PreferenceKey.CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL_SUBJECT.name());
		AsyncMailer asyncMailer = new AsyncMailer(monster.getLaboratorium().getBmhkLabWarnMail(), subject, mailContent)
		{
			@Override
			public void onError()
			{
				LogEvent logevent = new LogEvent(String.format("Email versturen bezwaar gebruik lichaamsmateriaal mislukt voor mosnterID %s", monster.getMonsterId()), Level.ERROR);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUREN_MISLUKT, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

			@Override
			public void onSuccess()
			{
				LogEvent logevent = new LogEvent(String.format("Nieuwe bezwaarmail verstuurd voor monsterID %s", monster.getMonsterId()), Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

		};
		mailService.sendAsyncMail(asyncMailer);
	}

	@Override
	public void sendBMHKBezwaarControlleVerwijsAdviesMail(CervixMonster monster)
	{
		long clientid = getClientIdVanMonster(monster);
		final String mailContent = getBaseMailContent(PreferenceKey.CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES, monster);
		final String subject = preferenceService.getString(PreferenceKey.CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES_SUBJECT.name());
		AsyncMailer asyncMailer = new AsyncMailer(monster.getLaboratorium().getBmhkLabWarnMail(), subject, mailContent)
		{
			@Override
			public void onError()
			{
				LogEvent logevent = new LogEvent(String.format("Email versturen bezwaar controle verwijs advies mislukt voor mosnterID %s", monster.getMonsterId()), Level.ERROR);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUREN_MISLUKT, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

			@Override
			public void onSuccess()
			{
				LogEvent logevent = new LogEvent(String.format("Nieuwe bezwaarmail (controle verwijs advies) verstuurd voor monsterID %s", monster.getMonsterId()), Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

		};
		mailService.sendAsyncMail(asyncMailer);
	}

	@Override
	public void sendOnbeoordeelbaarMaarTochOntvangstBeoordeling(CervixUitstrijkje uitstrijkje)
	{
		long clientid = getClientIdVanMonster(uitstrijkje);
		String content = getBaseMailContent(PreferenceKey.CERVIX_OMMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL, uitstrijkje);
		final String subject = preferenceService.getString(PreferenceKey.CERVIX_OMMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL_SUBJECT.name());
		AsyncMailer asyncMailer = new AsyncMailer(uitstrijkje.getLaboratorium().getBmhkLabWarnMail(), subject, content)
		{
			@Override
			public void onError()
			{
				LogEvent logevent = new LogEvent(String.format("Beoordeling ontvangen na verlopen omissie mail versturen mislukt. Voor monster id: %s", uitstrijkje.getMonsterId()),
					Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUREN_MISLUKT, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

			@Override
			public void onSuccess()
			{
				LogEvent logevent = new LogEvent(String.format("Beoordeling ontvangen na verlopen omissie mail verstuurd. Voor monster id: %s", uitstrijkje.getMonsterId()),
					Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}
		};
		mailService.sendAsyncMail(asyncMailer);
	}

	@Override
	public void sendWachttijdVerstrekenMetHuisartsOnbekend(CervixUitstrijkje uitstrijkje)
	{
		long clientid = getClientIdVanMonster(uitstrijkje);
		String content = getBaseMailContent(PreferenceKey.CERVIX_OMMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL, uitstrijkje);
		String subject = preferenceService.getString(PreferenceKey.CERVIX_OMMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL_SUBJECT.name());
		AsyncMailer asyncMailer = new AsyncMailer(uitstrijkje.getLaboratorium().getBmhkLabWarnMail(), subject, content)
		{
			@Override
			public void onError()
			{
				LogEvent logevent = new LogEvent(String.format("Wachttijd verstreken voor uitstrijkje mail versturen mislukt voor monster id: %s", uitstrijkje.getMonsterId()),
					Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUREN_MISLUKT, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

			@Override
			public void onSuccess()
			{
				LogEvent logevent = new LogEvent(String.format("Wachttijd verstreken voor uitstrijkje mail verstuurd voor monster id: %s", uitstrijkje.getMonsterId()), Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}
		};
		mailService.sendAsyncMail(asyncMailer);
	}

	@Override
	public void sendHuisartsGekoppeldAanUitstrijkjeMail(CervixUitstrijkje uitstrijkje)
	{
		long clientid = getClientIdVanMonster(uitstrijkje);
		String content = getBaseMailContent(PreferenceKey.CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL, uitstrijkje);
		String subject = preferenceService.getString(PreferenceKey.CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL_SUBJECT.name());
		CervixOmissiesLabproces omissie = appContext.getBean(CervixOmissiesLabproces.class, uitstrijkje);
		content = content.replaceAll("\\{aantalWerkdagenTotInsturen}", String.valueOf(omissie.omissieWachtOpCytologieUitslag().bepaalWerkdagenTotOmissie()));
		AsyncMailer asyncMailer = new AsyncMailer(uitstrijkje.getLaboratorium().getBmhkLabWarnMail(), subject, content)
		{
			@Override
			public void onError()
			{
				LogEvent logevent = new LogEvent(String.format("Huisarts gekoppeld aan uitstrijkje mail versturen is mislukt voor monster id: %s", uitstrijkje.getMonsterId()),
					Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUREN_MISLUKT, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}

			@Override
			public void onSuccess()
			{
				LogEvent logevent = new LogEvent(String.format("Huisarts gekoppeld aan uitstrijkje mail is verstuurd voor monster id: %s", uitstrijkje.getMonsterId()),
					Level.INFO);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, logevent, getClient(clientid), Bevolkingsonderzoek.CERVIX);
			}
		};
		mailService.sendAsyncMail(asyncMailer);
	}

	private String getBaseMailContent(PreferenceKey mailKey, CervixMonster monster)
	{
		String content = preferenceService.getString(mailKey.name());
		content = content.replaceAll("\\\\u00eb", "&euml;");
		content = content.replaceAll("\\u00eb", "&euml;");
		content = content.replaceAll("\\{monsterID}", monster.getMonsterId());
		content = content.replaceAll("\\{ordernummer}", monster.getMonsterId());
		content = content.replaceAll("\\{monsterOntvangst}", Constants.getDateFormat().format(monster.getOntvangstdatum()));
		content = content.replaceAll("\\{orderVerstuurd}", orderVerstuurdMergeField(monster));
		content = content.replaceAll("\\{datumverslag}", datumVerslagMergeField(monster));
		content = content.replaceAll("\\{gebdatum}", getGeboortedatumClient(monster));
		content = content.replaceAll("\\{datumOnbeoordeelbaarBrief}", getBriefDatumMergeField(monster));
		content = content.replaceAll("\\{huidigeDatum}", Constants.getDateFormat().format(currentDateSupplier.getDate()));
		return content;
	}

	private String getBriefDatumMergeField(CervixMonster monster)
	{
		return monster.getBrief() != null ? Constants.getDateFormat().format(monster.getBrief().getCreatieDatum()) : "";
	}

	private String datumVerslagMergeField(CervixMonster monster)
	{
		String datumVerslag = "";
		if (monster instanceof CervixUitstrijkje)
		{
			CervixUitstrijkje cervixUitstrijkje = (CervixUitstrijkje) monster;
			datumVerslag = cervixUitstrijkje.getCytologieVerslag() != null && cervixUitstrijkje.getCytologieVerslag().getDatumVerwerkt() != null
				? Constants.getDateFormat().format(cervixUitstrijkje.getCytologieVerslag().getDatumVerwerkt())
				: "";
		}
		return datumVerslag;
	}

	private String getGeboortedatumClient(CervixMonster monster)
	{
		return Constants.getDateFormat().format(monster.getOntvangstScreeningRonde().getDossier().getClient().getPersoon().getGeboortedatum());
	}

	private String orderVerstuurdMergeField(CervixMonster monster)
	{
		String orderVerstuurd = "";
		if (monster instanceof CervixUitstrijkje)
		{
			CervixUitstrijkje cervixUitstrijkje = (CervixUitstrijkje) monster;
			orderVerstuurd = cervixUitstrijkje.getCytologieOrder() != null ? Constants.getDateFormat().format(cervixUitstrijkje.getCytologieOrder().getStatusDatum()) : "";
		}
		return orderVerstuurd;
	}

	private Long getClientIdVanMonster(CervixMonster monster)
	{
		return monster.getOntvangstScreeningRonde().getDossier().getClient().getId();
	}
}
