package nl.rivm.screenit.main.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;

import nl.rivm.screenit.model.enums.LogGebeurtenis;

public enum MammaImsErrorType
{

	TIMEOUT(new String[] { "timeout" }, "error.mammobridge.http.timeout", LogGebeurtenis.MAMMA_BE_IMS_HTTP_FOUT),
	ERROR(new String[] { "error" }, "error.mammobridge.http.error", LogGebeurtenis.MAMMA_BE_IMS_HTTP_FOUT),
	WEBSOCKET(new String[] { "websocketError", "websocket" }, "error.mammobridge.ws.timeout", LogGebeurtenis.MAMMA_BE_IMS_WS_FOUT),
	OUT_OF_SYNC(new String[] { "outofsync" }, "error.mammobridge.outofsync", LogGebeurtenis.MAMMA_BE_IMS_HTTP_FOUT),
	USERNAME_INCORRECT(new String[] { "username-incorrect" }, "error.mammobridge.ws.username-incorrect", LogGebeurtenis.MAMMA_BE_IMS_WS_FOUT),
	ONBEKENDE_CLIENT(new String[] { "Patient ID in PACS != Patient ID in ScreenIT", " " }, "error.mammobridge.http.onbekendeclient", LogGebeurtenis.MAMMA_BE_IMS_WS_FOUT),
	ONBEKEND(new String[] { "onbekend" }, "error.mammobridge.unknown", LogGebeurtenis.MAMMA_BE_IMS_ERROR_ONBEKEND);

	private String[] errorCodes;

	private String meldingProperty;

	private LogGebeurtenis logGebeurtenis;

	public String getMeldingProperty()
	{
		return this.meldingProperty;
	}

	public LogGebeurtenis getLogGebeurtenis()
	{
		return logGebeurtenis;
	}

	public String[] getErrorCodes()
	{
		return errorCodes;
	}

	public static MammaImsErrorType findForCode(String code)
	{
		return Arrays.stream(MammaImsErrorType.values())
			.filter(errorType -> Arrays.stream(errorType.errorCodes)
				.anyMatch(errorCode -> errorCode.equalsIgnoreCase(code)))
			.findFirst()
			.orElse(ONBEKEND);
	}

	MammaImsErrorType(String[] errorCodes, String meldingProperty, LogGebeurtenis logGebeurtenis)
	{
		this.errorCodes = errorCodes;
		this.meldingProperty = meldingProperty;
		this.logGebeurtenis = logGebeurtenis;
	}
}
