package nl.rivm.screenit.service.impl;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.PreferenceKey;

@AllArgsConstructor
@Getter
enum MedewerkerMailTemplateConfig
{

	WACHTWOORD_VERLOOPT_HERINNERNIG(PreferenceKey.WACHTWOORDVERLOOPTEMAILSUBJECT, Constants.DEFAULT_GEBRUIKER_WACHTWOORD_VERLOOPT_MAIL_SUBJECT,
		PreferenceKey.WACHTWOORDVERLOOPTEMAIL, Constants.DEFAULT_GEBRUIKER_WACHTWOORD_VERLOOPT_MAIL_CONTENT,
		false, ""),
	BMHK_HUISARTS_REGISTRATIE(PreferenceKey.HUISARTS_REG_EMAILSUBJECT, Constants.DEFAULT_HA_REGISTRATIE_MAIL_SUBJECT,
		PreferenceKey.HUISARTS_REG_EMAIL, Constants.DEFAULT_HA_REGISTRATIE_MAIL_CONTENT,
		true, "registreren"),

	BMHK_HUISARTS_WACHTWOORD_VERGETEN(PreferenceKey.HUISARTS_WACHTWOORD_EMAILSUBJECT, Constants.DEFAULT_HA_WACHTWOORD_VERGETEN_MAIL_SUBJECT,
		PreferenceKey.HUISARTS_WACHTWOORD_EMAIL, Constants.DEFAULT_HA_WACHTWOORD_VERGETEN_MAIL_CONTENT,
		true, "wachtwoordvergeten/registreren");

	@NoArgsConstructor(access = AccessLevel.PRIVATE)
	static class Constants
	{
		static final String DEFAULT_HA_REGISTRATIE_MAIL_SUBJECT = "Registreren huisarts";

		static final String DEFAULT_HA_REGISTRATIE_MAIL_CONTENT = "U kunt zich registreren als huisarts met deze {link} met code {code}.";

		static final String DEFAULT_HA_WACHTWOORD_VERGETEN_MAIL_SUBJECT = "Huisartsenportaal - Wachtwoord vergeten";

		static final String DEFAULT_HA_WACHTWOORD_VERGETEN_MAIL_CONTENT = "Geachte {aanhef}{tussenvoegsel}{achternaam} <br><br>"
			+ "U heeft een nieuw wachtwoord aangevraagd. U kunt via deze {link} en inlogcode: {code} uw wachtwoord opnieuw instellen. <br><br> "
			+ "Met vriendelijke groet, <br> Het ScreenIT-team";

		static final String DEFAULT_GEBRUIKER_WACHTWOORD_VERLOOPT_MAIL_SUBJECT = "Wachtwoord verloopt";

		static final String DEFAULT_GEBRUIKER_WACHTWOORD_VERLOOPT_MAIL_CONTENT = "Geachte {aanhef}{tussenvoegsel}{achternaam} <br><br>"
			+ "Over een aantal dagen verloopt uw wachtwoord. U kunt via deze {link} uw wachtwoord opnieuw instellen. <br><br> "
			+ "Met vriendelijke groet, <br> Het ScreenIT-team";
	}

	private final PreferenceKey subjectTemplate;

	private final String defaultSubjectTemplate;

	private final PreferenceKey contentTemplate;

	private final String defaultContentTemplate;

	private final boolean useHuisartsenPortaalUrl;

	private final String urlContext;
}
