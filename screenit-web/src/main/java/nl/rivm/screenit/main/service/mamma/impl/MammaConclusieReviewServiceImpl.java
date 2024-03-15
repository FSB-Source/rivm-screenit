package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dao.mamma.MammaConclusieReviewDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.repository.mamma.MammaConclusieReviewRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MammaConclusieReviewServiceImpl implements MammaConclusieReviewService
{
	private final MammaConclusieReviewDao conclusieReviewDao;

	private final MammaConclusieReviewRepository conclusieReviewRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final MammaConclusieReviewDataProviderServiceImpl conclusieReviewDataProviderService;

	private static final int DAGEN_VOOR_DATUM_CONCLUSIEREVIEW_FILTER = 30;

	@Override
	public MammaConclusieReview getConclusieReview(MammaScreeningRonde screeningRonde, InstellingGebruiker radioloog)
	{
		return conclusieReviewRepository.findByRadioloogAndScreeningRondeAndReviewAlsCoordinerendRadioloog(radioloog, screeningRonde, false)
			.orElse(null);
	}

	@Override
	public MammaConclusieReview getConclusieReviewCoordinerendRadioloog(MammaScreeningRonde screeningRonde, InstellingGebruiker coordinerendRadioloog)
	{
		return conclusieReviewRepository.findByRadioloogAndScreeningRondeAndReviewAlsCoordinerendRadioloog(coordinerendRadioloog, screeningRonde, true)
			.orElseGet(this::maakConclusieReviewCoordinerendRadioloog);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveConclusieReviewCoordinerendRadioloog(MammaConclusieReview conclusieReview, MammaScreeningRonde screeningRonde,
		InstellingGebruiker coordinerendRadioloog)
	{
		if (conclusieReview.getId() == null)
		{
			slaNieuweConclusieReviewOp(conclusieReview, screeningRonde, coordinerendRadioloog);
		}
		else
		{
			updateConclusieReview(conclusieReview);
		}

		logConclusieReviewAfgerond(coordinerendRadioloog, screeningRonde.getDossier().getClient(), conclusieReview, true);
	}

	private void slaNieuweConclusieReviewOp(MammaConclusieReview conclusieReview, MammaScreeningRonde screeningRonde, InstellingGebruiker coordinerendRadioloog)
	{
		conclusieReview.setRadioloog(coordinerendRadioloog);
		conclusieReview.setScreeningRonde(screeningRonde);
		screeningRonde.getConclusieReviews().add(conclusieReview);
		conclusieReview.setReviewMoment(currentDateSupplier.getLocalDateTime());

		hibernateService.saveOrUpdateAll(screeningRonde, conclusieReview);
	}

	private void updateConclusieReview(MammaConclusieReview conclusieReview)
	{
		conclusieReview.setReviewMoment(currentDateSupplier.getLocalDateTime());

		hibernateService.saveOrUpdate(conclusieReview);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakConclusieReviewVoorBetrokkenRadiologen(MammaScreeningRonde screeningRonde)
	{
		if (MammaFollowUpConclusieStatus.conclusieReviewStatussen().contains(screeningRonde.getFollowUpConclusieStatus()))
		{
			conclusieReviewDao.getRadiologenMetLezingVanRondeEnZonderReview(screeningRonde).forEach(r -> maakConclusieReview(r, screeningRonde));
		}
	}

	private void maakConclusieReview(InstellingGebruiker gebruiker, MammaScreeningRonde screeningRonde)
	{
		if (getConclusieReview(screeningRonde, gebruiker) == null)
		{
			var conclusieReview = new MammaConclusieReview();
			conclusieReview.setScreeningRonde(screeningRonde);
			conclusieReview.setRadioloog(gebruiker);
			screeningRonde.getConclusieReviews().add(conclusieReview);

			hibernateService.saveOrUpdateAll(conclusieReview, screeningRonde);
		}
	}

	private MammaConclusieReview maakConclusieReviewCoordinerendRadioloog()
	{
		var conclusieReview = new MammaConclusieReview();
		conclusieReview.setReviewAlsCoordinerendRadioloog(true);

		return conclusieReview;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void conclusieReviewAfronden(MammaConclusieReview conclusieReview)
	{
		conclusieReview.setReviewMoment(currentDateSupplier.getLocalDateTime());
		hibernateService.saveOrUpdate(conclusieReview);
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

	@Override
	public Date bepaalInitieleConclusieReviewSorteerDatumCoordinerendRadioloog(MammaConclusieReviewZoekObject zoekObject)
	{
		var laatsteConclusieReviewDatumRadioloog = conclusieReviewDataProviderService.findPage(0, 1, zoekObject,
			Sort.by(Sort.Direction.DESC, "screeningRonde.followUpConclusieStatusGewijzigdOp"));

		return laatsteConclusieReviewDatumRadioloog.isEmpty() ? DateUtil.minDagen(currentDateSupplier.getDate(), DAGEN_VOOR_DATUM_CONCLUSIEREVIEW_FILTER) :
			DateUtil.minDagen(laatsteConclusieReviewDatumRadioloog.get(0).getScreeningRonde().getFollowUpConclusieStatusGewijzigdOp(), DAGEN_VOOR_DATUM_CONCLUSIEREVIEW_FILTER);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logConclusieReviewAfgerond(InstellingGebruiker gebruiker, Client client, MammaConclusieReview conclusieReview, boolean doorCoordinerendRadioloog)
	{
		if (!doorCoordinerendRadioloog)
		{
			if (conclusieReview.getRetourCeReden() != null)
			{
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELINGEN_REVIEW_RETOUR_CE, gebruiker,
					client, conclusieReview.getRetourCeReden().getOmschrijving(), Bevolkingsonderzoek.MAMMA);
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_REVIEW_AFGEROND, gebruiker, client, Bevolkingsonderzoek.MAMMA);
			}
		}
		else
		{
			var logStringRadioloogRedenen = maakRedenenFotobesprekingLogString(conclusieReview.getRedenenFotobesprekingRadioloog(), ", rad:");
			var logStringMBBRedenen = maakRedenenFotobesprekingLogString(conclusieReview.getRedenenFotobesprekingMbber(), ", mbb:");

			var logMelding = String.format("Co\u00F6rdinerend radioloog: inzage ronde review%s%s", logStringRadioloogRedenen, logStringMBBRedenen);

			logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_REVIEW_AFGEROND, gebruiker,
				client, logMelding, Bevolkingsonderzoek.MAMMA);
		}
	}

	@Override
	public Optional<MammaConclusieReview> getReviewAfgerondDoorCoordinerendRadioloog(InstellingGebruiker coordinerendRadioloog, MammaScreeningRonde screeningRonde)
	{
		return conclusieReviewRepository.findByRadioloogAndScreeningRondeAndReviewAlsCoordinerendRadioloog(coordinerendRadioloog, screeningRonde, true);
	}

	private String maakRedenenFotobesprekingLogString(List<? extends INaam> redenenLijst, String beginString)
	{
		return redenenLijst.isEmpty() ? "" : redenenLijst.stream().map(INaam::getNaam).collect(Collectors.joining(", ", beginString + " ", ""));
	}
}
