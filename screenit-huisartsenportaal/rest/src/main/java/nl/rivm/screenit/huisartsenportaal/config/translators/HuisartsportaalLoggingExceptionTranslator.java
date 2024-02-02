package nl.rivm.screenit.huisartsenportaal.config.translators;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.oauth2.common.exceptions.OAuth2Exception;
import org.springframework.security.oauth2.provider.error.DefaultWebResponseExceptionTranslator;

public class HuisartsportaalLoggingExceptionTranslator extends DefaultWebResponseExceptionTranslator
{

	private static final Logger LOG = LoggerFactory.getLogger(HuisartsportaalLoggingExceptionTranslator.class);

	@Override
	public ResponseEntity<OAuth2Exception> translate(Exception e) throws Exception
	{
		ResponseEntity<OAuth2Exception> response = super.translate(e);

		if (e instanceof AuthenticationException)
		{
			LOG.debug("AuthenticatieException opgetreden: " + e.getMessage(), e);
		}
		LOG.error("Er is een onbekende fout opgtreden bij de Authenticatie: " + e.getMessage(), e);
		return response;
	}
}
