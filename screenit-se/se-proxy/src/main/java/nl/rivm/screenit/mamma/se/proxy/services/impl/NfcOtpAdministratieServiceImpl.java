package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.proxy.model.TimeCleanableItem;
import nl.rivm.screenit.mamma.se.proxy.services.NfcOtpAdministratieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class NfcOtpAdministratieServiceImpl implements NfcOtpAdministratieService
{
	private static final Logger LOG = LoggerFactory.getLogger(NfcOtpAdministratieServiceImpl.class);

	private Map<String, OtpEntry> laatsteSuccesvolleOtpPerPublicId = new ConcurrentHashMap<>();

	@Override
	public void setLaatsteSuccesvolleOtp(String publicId, String nieuweOtp)
	{
		laatsteSuccesvolleOtpPerPublicId.put(publicId, new OtpEntry(nieuweOtp));
	}

	@Override
	public boolean zelfdeOtpAlsLaatsteSuccesvolle(String publicId, String otp)
	{
		OtpEntry laatsteSuccesvolleOtpEntry = laatsteSuccesvolleOtpPerPublicId.get(publicId);
		return laatsteSuccesvolleOtpEntry != null && laatsteSuccesvolleOtpEntry.getOtp().equals(otp);
	}

	@Override
	public void clearOldEntries()
	{
		LOG.info("Verwijder opgeslagen OTP's ouder dan afgelopen middernacht");
		laatsteSuccesvolleOtpPerPublicId.entrySet().removeIf(e -> e.getValue().isOuderDan(DateUtil.getAfgelopenMiddernacht()));
	}

	private static class OtpEntry extends TimeCleanableItem
	{
		private final String otp;

		private OtpEntry(String otp)
		{
			this.otp = otp;
		}

		public String getOtp()
		{
			return otp;
		}

	}
}
