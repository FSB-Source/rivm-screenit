package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.dao.cervix.CervixLabformulierDao;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixLabformulierServiceImpl implements CervixLabformulierService
{

	private CervixLabformulierDao labformulierDao;

	private CervixVervolgService vervolgService;

	private HibernateService hibernateService;

	private CervixBaseMonsterService monsterService;

	private CervixDossierDao dossierDao;

	@Override
	public List<CervixLabformulier> getLabformulieren(CervixLabformulierenFilter filter, long first, long count, String sortProperty, boolean asc)
	{
		return labformulierDao.getLabformulieren(filter, first, count, sortProperty, asc);
	}

	@Override
	public int countLabformulieren(CervixLabformulierenFilter filter)
	{
		return labformulierDao.countLabformulieren(filter);
	}

	@Override
	public List<Long> getLabformulierenIds(CervixLabformulierenFilter filter, String sortProperty, boolean asc)
	{
		return labformulierDao.getLabformulierenIds(filter, sortProperty, asc);
	}

	@Override
	@Transactional(propagation = Propagation.NEVER)
	public void valideerLabformulier(CervixLabformulier labformulier)
	{
		if (labformulier.getStatus().equals(CervixLabformulierStatus.HUISARTS_ONBEKEND) || labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
			|| labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD))
		{
			valideerLabformulierKoppeling(labformulier);
		}
		valideerLabformulierWaarden(labformulier);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String koppelEnBewaarLabformulier(CervixLabformulier labformulier)
	{
		labformulierKoppelen(labformulier);
		valideerLabformulierWaarden(labformulier);

		if (StringUtils.isNotBlank(labformulier.getAspectCervixAbnormaalOfVerdachtePortioTekst()))
		{
			labformulier.setAspectCervixAbnormaalOfVerdachtePortio(true);
		}
		if (StringUtils.isNotBlank(labformulier.getGebruikHormonenJaVanwegeTekst()))
		{
			labformulier.setGebruikHormonenJaVanwege(true);
		}
		if (StringUtils.isNotBlank(labformulier.getKlachtenAndersNamelijkTekst()))
		{
			labformulier.setKlachtenAndersNamelijk(true);
		}
		if (StringUtils.isNotBlank(labformulier.getOpmerkingenTekst()))
		{
			labformulier.setOpmerkingen(true);
		}
		hibernateService.saveOrUpdate(labformulier);
		return EntityAuditUtil.getDiffToLatestVersion(labformulier, hibernateService.getHibernateSession());
	}

	private void valideerLabformulierWaarden(CervixLabformulier labformulier)
	{
		var scanDatum = labformulier.getScanDatum();
		var datumUitstrijkje = labformulier.getDatumUitstrijkje();

		if ((labformulier.getStatus().equals(CervixLabformulierStatus.HUISARTS_ONBEKEND) || labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
			|| labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD)) && labformulier.getDatumUitstrijkje() != null
			&& DateUtil.startDag(scanDatum).before(DateUtil.startDag(datumUitstrijkje)))
		{
			throw new IllegalStateException("datum.uitstrijkje.niet.voor.scan.datum");
		}
		if ((labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD) || labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE))
			&& labformulier.getHuisartsLocatie() == null)
		{
			throw new IllegalStateException("huisarts.onbekend");
		}
		if (labformulier.getStatus().equals(CervixLabformulierStatus.HUISARTS_ONBEKEND)
			&& labformulier.getHuisartsLocatie() != null)
		{
			throw new IllegalStateException("huisarts.bekend");
		}
	}

	private void labformulierKoppelen(CervixLabformulier labformulier) throws IllegalStateException
	{
		CervixUitstrijkje uitstrijkje = null;
		try
		{
			uitstrijkje = valideerLabformulierKoppeling(labformulier);
		}
		catch (IllegalStateException e)
		{
			if (labformulier.getStatus().equals(CervixLabformulierStatus.HUISARTS_ONBEKEND) || labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
				|| labformulier.getStatus().equals(CervixLabformulierStatus.GECONTROLEERD))
			{

				throw e;
			}
			else
			{

				CervixUitstrijkje eerderUitstrijkje = labformulier.getUitstrijkje();
				if (eerderUitstrijkje != null)
				{

					eerderUitstrijkje.setLabformulier(null);
					labformulier.setUitstrijkje(null);

					eerderUitstrijkje.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(eerderUitstrijkje));
					hibernateService.saveOrUpdate(eerderUitstrijkje);
				}
				return;
			}
		}

		CervixUitstrijkje eerderUitstrijkje = labformulier.getUitstrijkje();
		if (eerderUitstrijkje != null && !uitstrijkje.equals(eerderUitstrijkje))
		{

			eerderUitstrijkje.setLabformulier(null);

			eerderUitstrijkje.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(eerderUitstrijkje));
			hibernateService.saveOrUpdate(eerderUitstrijkje);
		}
		labformulier.setUitstrijkje(uitstrijkje);
		uitstrijkje.setLabformulier(labformulier);

		uitstrijkje.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(uitstrijkje));
		hibernateService.saveOrUpdate(uitstrijkje);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void koppelDigitaalLabformulier(CervixLabformulier labformulier)
	{
		CervixUitstrijkje uitstrijkje = labformulier.getUitstrijkje();
		uitstrijkje.setLabformulier(labformulier);
		uitstrijkje.setOntvangstScreeningRonde(dossierDao.getOntvangstRonde(uitstrijkje));
		hibernateService.saveOrUpdate(labformulier);
		vervolgService.digitaalLabformulierKlaarVoorCytologie(uitstrijkje);
	}

	@Override
	public void updateLabformulierLaboratoriumNaOntvangstMonster(CervixMonster monster)
	{
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{
			var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
			var laboratorium = uitstrijkje.getLaboratorium();
			var labformulier = uitstrijkje.getLabformulier();
			if (labformulier != null && laboratorium != null && !laboratorium.equals(labformulier.getLaboratorium()) && Boolean.TRUE.equals(labformulier.getDigitaal()))
			{
				labformulier.setLaboratorium(laboratorium);
				hibernateService.saveOrUpdate(labformulier);
			}

		}
	}

	private CervixUitstrijkje valideerLabformulierKoppeling(CervixLabformulier labformulier)
	{
		CervixUitstrijkje uitstrijkje;
		if (StringUtils.isBlank(labformulier.getBarcode()))
		{
			throw new IllegalStateException("monsterId.leeg");
		}
		else
		{
			uitstrijkje = monsterService.getUitstrijkje(labformulier.getBarcode()).orElse(null);
			if (uitstrijkje == null)
			{
				throw new IllegalStateException("geen.uitnodiging.gevonden");
			}
			else
			{
				MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(uitstrijkje.getUitnodiging().getBrief());
				if (mergedBrieven == null || mergedBrieven.getPrintDatum() == null)
				{
					throw new IllegalStateException("geen.verzonden.uitnodiging.gevonden");
				}
				else
				{
					CervixLabformulier anderLabformulier = uitstrijkje.getLabformulier();
					if (anderLabformulier != null && !labformulier.equals(anderLabformulier))
					{
						throw new IllegalStateException("ander.labformulier.gekoppeld");
					}
					else
					{
						dossierDao.getOntvangstRonde(uitstrijkje);
					}
				}
			}
		}
		return uitstrijkje;
	}
}
