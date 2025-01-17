package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.dto.SeStatusDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheidStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaScreeningsEenheidStatusServiceImpl implements MammaScreeningsEenheidStatusService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaScreeningsEenheidStatusServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaScreeningsEenheidService mammaScreeningsEenheidService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public void verwerkStatusBericht(MammaScreeningsEenheid screeningsEenheid, SeStatusDto statusDto)
	{
		MammaScreeningsEenheidStatus status = screeningsEenheid.getStatus();
		if (status == null)
		{
			status = new MammaScreeningsEenheidStatus();
			status.setScreeningsEenheid(screeningsEenheid);
			screeningsEenheid.setStatus(status);
		}
		status.setVersie(statusDto.getVersie());
		status.setHuisartsenAanwezig(statusDto.isHuisartsenAanwezig());
		status.setZorginstellingenAanwezig(statusDto.isZorginstellingenAanwezig());
		status.setMammografenAanwezig(statusDto.isMammografenAanwezig());
		status.setOfflineDaglijsten(StringUtils.left(statusDto.getOfflineDaglijsten(), 255));
		status.setStatusMoment(statusDto.getStatusMoment());
		status.setLaatsteKeerOfflineGegaan(null);
		hibernateService.saveOrUpdateAll(status, screeningsEenheid);
	}

	@Override
	public void slaVerbindingStatusOp(String seCode, boolean online)
	{
		MammaScreeningsEenheid screeningsEenheid = mammaScreeningsEenheidService.getActieveScreeningsEenheidByCode(seCode);
		if (screeningsEenheid == null)
		{
			LOG.warn("Statusupdate van onbekende SE ({}) ontvangen", seCode);
			return;
		}
		MammaScreeningsEenheidStatus status = screeningsEenheid.getStatus();
		if (status == null)
		{
			LOG.warn("SE {} heeft nog geen status", seCode);
			return;
		}
		status.setLaatsteKeerOfflineGegaan(online ? null : currentDateSupplier.getLocalDateTime());
		hibernateService.saveOrUpdate(status);
	}

}
