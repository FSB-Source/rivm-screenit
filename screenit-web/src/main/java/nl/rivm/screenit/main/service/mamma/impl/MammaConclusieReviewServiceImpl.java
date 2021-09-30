package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaConclusieReviewDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaConclusieReviewServiceImpl implements MammaConclusieReviewService
{
	@Autowired
	private MammaConclusieReviewDao conclusieReviewDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public long countScreeningRondesMetConclusie(MammaConclusieReviewZoekObject zoekObject)
	{
		return conclusieReviewDao.countScreeningRondesMetConclusie(zoekObject);
	}

	@Override
	public List<MammaScreeningRonde> zoekScreeningRondesMetConclusie(MammaConclusieReviewZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		return conclusieReviewDao.zoekScreeningRondesMetConclusie(zoekObject, first, count, sortProperty, asc);
	}

	@Override
	public List<Long> zoekBeoordelingIdsMetConclusie(MammaConclusieReviewZoekObject zoekObject, String sortProperty, boolean asc)
	{
		return conclusieReviewDao.zoekBeoordelingIdsMetConclusie(zoekObject, sortProperty, asc);
	}

	@Override
	public LocalDateTime getReviewMoment(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog)
	{
		return screeningRonde.getConclusieReviews().stream().filter(review -> review.getRadioloog().equals(radioloog)).findFirst()
			.map(MammaConclusieReview::getReviewMoment).orElse(null);
	}

	@Override
	public MammaConclusieReview getConclusieReview(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog)
	{
		return conclusieReviewDao.getConclusieReview(screeningRonde, radioloog);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveConclusieReview(InstellingGebruiker gebruiker, MammaScreeningRonde screeningRonde, MammaConclusieReview conclusieReview)
	{
		conclusieReview.setReviewMoment(currentDateSupplier.getLocalDateTime());
		if (conclusieReview.getScreeningRonde() == null)
		{
			conclusieReview.setScreeningRonde(screeningRonde);
			conclusieReview.setRadioloog(gebruiker);
			screeningRonde.getConclusieReviews().add(conclusieReview);
		}
		hibernateService.saveOrUpdateAll(conclusieReview, screeningRonde);
	}

	@Override
	public MammobridgeRole getMammobridgeRoleBijConclusieReviewFilter(MammaConclusieReviewFilterOptie filterOptie)
	{
		if (MammaConclusieReviewFilterOptie.FALSE_NEGATIVE.equals(filterOptie)
			|| MammaConclusieReviewFilterOptie.FALSE_NEGATIVE_MBB_SIGNALERING.equals(filterOptie))
		{
			return MammobridgeRole.IC_T2;
		}
		else
		{
			return MammobridgeRole.RADIOLOGIST;
		}
	}
}
