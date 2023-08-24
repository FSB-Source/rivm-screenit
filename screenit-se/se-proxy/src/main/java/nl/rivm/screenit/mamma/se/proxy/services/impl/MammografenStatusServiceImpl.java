package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import nl.rivm.screenit.mamma.se.proxy.model.MammograafStatus;
import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.dcm4che3.net.Association;
import org.springframework.stereotype.Service;

@Service
public class MammografenStatusServiceImpl implements MammografenStatusService
{

	public static final int MAXIMAAL_AANTAL_GETOONDE_DICOM_FOUTEN = 10;

	private ConcurrentMap<String, MammograafStatus> mammograafStatusByAeTitle = new ConcurrentHashMap<>();

	@Override
	public void registreerLaatstSuccesvolleDmwlBerichtVanMammograaf(Association as)
	{
		MammograafStatus status = getMammograafDicomStatus(as.getCallingAET());
		status.setLaatsteSuccesDmwlBerichtTimestamp(DateUtil.getCurrentDateTime());
		status.getFoutenSindsLaatsteSuccesDmwlBericht().clear();
	}

	@Override
	public void registreerDmwlFout(Association as, String foutMelding)
	{
		MammograafStatus status = getMammograafDicomStatus(as.getCallingAET());
		if (status.getFoutenSindsLaatsteSuccesDmwlBericht().size() >= MAXIMAAL_AANTAL_GETOONDE_DICOM_FOUTEN)
		{
			status.getFoutenSindsLaatsteSuccesDmwlBericht().remove(0);
		}
		status.registreerDmwlFout(foutMelding);
	}

	@Override
	public void registreerMammograafDatum(Association as, String datum)
	{
		MammograafStatus status = getMammograafDicomStatus(as.getCallingAET());
		status.setMammograafDatum(datum);
	}

	@Override
	public void registreerLaatstSuccesvolleMppsBerichtVanMammograaf(Association as)
	{
		MammograafStatus status = getMammograafDicomStatus(as.getCallingAET());
		status.setLaatsteSuccesMppsBerichtTimestamp(DateUtil.getCurrentDateTime());
		status.getFoutenSindsLaatsteSuccesMppsBericht().clear();
	}

	@Override
	public void registreerMppsFout(Association as, String foutMelding)
	{
		MammograafStatus status = getMammograafDicomStatus(as.getCallingAET());
		if (status.getFoutenSindsLaatsteSuccesMppsBericht().size() >= MAXIMAAL_AANTAL_GETOONDE_DICOM_FOUTEN)
		{
			status.getFoutenSindsLaatsteSuccesMppsBericht().remove(0);
		}
		status.registreerMppsFout(foutMelding);
	}

	@Override
	public MammograafStatus getMammograafDicomStatus(String aeTitle)
	{
		mammograafStatusByAeTitle.putIfAbsent(aeTitle, new MammograafStatus(aeTitle));
		return mammograafStatusByAeTitle.get(aeTitle);
	}

}
