package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
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

	private static final String BASIC_LOGMELDING_VERSTUURD_SUFFIX = " verstuurd voor monsterId '%s'";

	@Override
	public void sendBMHKBezwaarLichaamsmateriaalMailAsync(CervixMonster monster)
	{
		final var content = getBaseMailContent(PreferenceKey.CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL, monster);
		final var subject = preferenceService.getString(PreferenceKey.CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL_SUBJECT.name());

		var basicMelding = "Nieuwe bezwaarmail" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, monster);
	}

	@Override
	public void sendBMHKBezwaarControlleVerwijsAdviesMail(CervixMonster monster)
	{
		final var content = getBaseMailContent(PreferenceKey.CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES, monster);
		final var subject = preferenceService.getString(PreferenceKey.CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES_SUBJECT.name());

		var basicMelding = "Nieuwe bezwaarmail (controle verwijs advies)" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, monster);
	}

	@Override
	public void sendOnbeoordeelbaarMaarTochOntvangstBeoordeling(CervixUitstrijkje uitstrijkje)
	{
		var content = getBaseMailContent(PreferenceKey.CERVIX_OMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL, uitstrijkje);
		final var subject = preferenceService.getString(PreferenceKey.CERVIX_OMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL_SUBJECT.name());

		var basicMelding = "Beoordeling ontvangen na verlopen omissie mail" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, uitstrijkje);
	}

	@Override
	public void sendWachttijdVerstrekenMetHuisartsOnbekend(CervixUitstrijkje uitstrijkje)
	{
		var content = getBaseMailContent(PreferenceKey.CERVIX_OMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL, uitstrijkje);
		var subject = preferenceService.getString(PreferenceKey.CERVIX_OMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL_SUBJECT.name());

		var basicMelding = "Wachttijd verstreken voor uitstrijkje mail" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, uitstrijkje);
	}

	@Override
	public void sendHuisartsGekoppeldAanUitstrijkjeMail(CervixUitstrijkje uitstrijkje)
	{
		var content = getBaseMailContent(PreferenceKey.CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL, uitstrijkje);
		var subject = preferenceService.getString(PreferenceKey.CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL_SUBJECT.name());
		CervixOmissiesLabproces omissie = appContext.getBean(CervixOmissiesLabproces.class, uitstrijkje);
		content = content.replace("{aantalWerkdagenTotInsturen}", String.valueOf(omissie.omissieWachtOpCytologieUitslag().bepaalWerkdagenTotOmissie()));

		var basicMelding = "Huisarts gekoppeld aan uitstrijkje mail is" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, uitstrijkje);
	}

	@Override
	public void sendWachtOpCytologieUitslagMail(CervixMonster monster)
	{
		var content = getBaseMailContent(PreferenceKey.CERVIX_OMISSIE_ONTBREKEND_CYTOLOGIEVERSLAG_MAIL, monster);
		var subject = preferenceService.getString(PreferenceKey.CERVIX_OMISSIE_ONTBREKEND_CYTOLOGIEVERSLAG_SUBJECT.name());
		var wachtOp = preferenceService.getInteger(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_CYTOLOGIE_UITSLAG.name());
		var wachtOpWaarschuwing = preferenceService.getInteger(PreferenceKey.CERVIX_WACHTTIJD_WACHT_OP_WAARSCHUWING_CYTOLOGIE_UITSLAG.name());
		content = content.replace("{wachtOpCytoUitslag}", wachtOp.toString());
		content = content.replace("{wachtOpWaarschuwingCytoUitslag}", wachtOpWaarschuwing.toString());

		var basicMelding = "Wacht op cytologie uitslag mail is" + BASIC_LOGMELDING_VERSTUURD_SUFFIX;
		stuurMailEnLog(subject, content, basicMelding, monster);
	}

	private void stuurMailEnLog(String mailSubject, String mailContent, String logMeldingNaMail, CervixMonster monster)
	{
		mailService.queueMailAanProfessional(monster.getLaboratorium().getBmhkLabWarnMail(), mailSubject, mailContent);
		logMailGestuurd(logMeldingNaMail, monster);
	}

	private void logMailGestuurd(String logMelding, CervixMonster monster)
	{
		var logMeldingVolledig = String.format(logMelding, monster.getMonsterId());
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_MAIL_VERSTUURD, getClientVanMonster(monster), logMeldingVolledig, Bevolkingsonderzoek.CERVIX);
	}

	private String getBaseMailContent(PreferenceKey mailKey, CervixMonster monster)
	{
		var content = preferenceService.getString(mailKey.name());

		content = content.replace("\\u00eb", "&euml;");
		content = content.replace("u00eb", "&euml;");
		content = content.replace("{monsterID}", monster.getMonsterId());
		content = content.replace("{ordernummer}", monster.getMonsterId());
		content = content.replace("{monsterOntvangst}", Constants.getDateFormat().format(monster.getOntvangstdatum()));
		content = content.replace("{orderVerstuurd}", orderVerstuurdMergeField(monster));
		content = content.replace("{datumverslag}", datumVerslagMergeField(monster));
		content = content.replace("{gebdatum}", getGeboortedatumClient(monster));
		content = content.replace("{datumOnbeoordeelbaarBrief}", getBriefDatumMergeField(monster));
		content = content.replace("{huidigeDatum}", Constants.getDateFormat().format(currentDateSupplier.getDate()));
		content = content.replace("{labNaam}", monster.getLaboratorium().getNaam());
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

	private Client getClientVanMonster(CervixMonster monster)
	{
		return monster.getOntvangstScreeningRonde().getDossier().getClient();
	}
}
