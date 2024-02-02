package nl.rivm.screenit.service.mamma.impl;

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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval;
import nl.rivm.screenit.model.mamma.MammaVolgendeUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitnodigingsintervalType;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaVolgendeUitnodigingServiceImpl implements MammaVolgendeUitnodigingService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	@Lazy
	private MammaBaseFactory baseFactory;

	@Autowired
	@Lazy
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private LogService logService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private MammaBaseFollowUpService followUpService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean isSuspect(MammaDossier dossier)
	{
		return MammaUitnodigingsintervalType.isSuspect(getVolgendeUitnodigingIntervalType(dossier));
	}

	private MammaUitnodigingsintervalType getVolgendeUitnodigingIntervalType(MammaDossier dossier)
	{
		return dossier.getVolgendeUitnodiging() != null ? dossier.getVolgendeUitnodiging().getInterval().getType() : null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean isSuspectOfHoogRisico(MammaDossier dossier)
	{
		return MammaUitnodigingsintervalType.isSuspectOfHoogRisico(getVolgendeUitnodigingIntervalType(dossier));
	}

	@Override
	public void updateVolgendeUitnodigingNaVerwijziging(MammaDossier dossier)
	{
		if (dossier.getLaatsteBeoordelingMetUitslag().getStatus() != MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)
		{
			throw new IllegalStateException("Laatste beoordeling is geen verwijzing voor client: " + dossier.getClient().getId());
		}

		var volgendeUitnodigingIntervalType = getVolgendeUitnodigingIntervalType(dossier);
		if (volgendeUitnodigingIntervalType == null)
		{
			var peildatum = DateUtil.toLocalDate(dossier.getLaatsteBeoordelingMetUitslag().getOnderzoek().getCreatieDatum());
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.SUSPECT, peildatum);
		}
		else if (volgendeUitnodigingIntervalType == MammaUitnodigingsintervalType.HOOG_RISICO_DEELNAME)
		{
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT, dossier.getVolgendeUitnodiging().getPeildatum());
		}
	}

	private void zetVolgendeUitnodigingsinterval(MammaDossier dossier, MammaUitnodigingsintervalType type, LocalDate peildatum)
	{
		var volgendeUitnodiging = dossier.getVolgendeUitnodiging();
		if (volgendeUitnodiging == null)
		{
			volgendeUitnodiging = new MammaVolgendeUitnodiging();
			volgendeUitnodiging.setDossier(dossier);
			dossier.setVolgendeUitnodiging(volgendeUitnodiging);
		}
		volgendeUitnodiging.setPeildatum(peildatum);
		volgendeUitnodiging.setInterval(getIntervalByType(type));
		hibernateService.saveOrUpdateAll(volgendeUitnodiging, dossier);
	}

	@Override
	public void updateVolgendeUitnodigingNaFollowUpConclusie(MammaScreeningRonde screeningRonde)
	{
		var conclusie = screeningRonde.getFollowUpConclusieStatus();
		if (conclusie == MammaFollowUpConclusieStatus.FALSE_NEGATIVE)
		{
			updateVolgendeUitnodigingNaFoutNegatief(screeningRonde);
		}
		else if (conclusie == MammaFollowUpConclusieStatus.TRUE_POSITIVE)
		{
			updateVolgendeUitnodigingNaTerechtPositief(screeningRonde);
		}
		else if (conclusie == MammaFollowUpConclusieStatus.FALSE_POSITIVE)
		{
			verlaatSuspectDoorDeelnameOfFoutPositief(screeningRonde.getDossier());
		}
	}

	private void updateVolgendeUitnodigingNaFoutNegatief(MammaScreeningRonde screeningRonde)
	{
		var dossier = screeningRonde.getDossier();
		var volgendeUitnodigingIntervalType = getVolgendeUitnodigingIntervalType(dossier);
		if (volgendeUitnodigingIntervalType == null)
		{
			var peildatum = initielePeildatumBijFoutNegatief(screeningRonde);
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT, peildatum);
		}
		else if (volgendeUitnodigingIntervalType != MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT)
		{
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT, dossier.getVolgendeUitnodiging().getPeildatum());
		}
	}

	private LocalDate initielePeildatumBijFoutNegatief(MammaScreeningRonde screeningRonde)
	{
		var peildatum = followUpService.getEersteAutorisatieDatumPaVerslag(screeningRonde);
		if (peildatum == null)
		{
			peildatum = laatsteOnderzoeksDatumVanRonde(screeningRonde);
			LOG.warn("Kon geen PA autorisatiedatum vinden bij Fout Negatief voor ronde {}. Datum laatste onderzoek van ronde gebruikt als peildatum ", screeningRonde.getId());
		}
		return corrigeerInitielePeildatumVoorNieuwereUitnodiging(screeningRonde.getDossier(), peildatum);
	}

	private LocalDate laatsteOnderzoeksDatumVanRonde(MammaScreeningRonde screeningRonde)
	{
		return DateUtil.toLocalDate(screeningRonde.getLaatsteOnderzoek().getCreatieDatum());
	}

	private LocalDate corrigeerInitielePeildatumVoorNieuwereUitnodiging(MammaDossier dossier, LocalDate gewenstePeildatum)
	{
		var uitnodigingsDatumLaatsteRonde = datumEersteUitnodingsbriefVanLaatsteRonde(dossier);
		return Stream.of(gewenstePeildatum, uitnodigingsDatumLaatsteRonde).filter(Objects::nonNull).max(Comparator.naturalOrder()).orElseThrow();
	}

	private void updateVolgendeUitnodigingNaTerechtPositief(MammaScreeningRonde screeningRonde)
	{
		var dossier = screeningRonde.getDossier();
		var volgendeUitnodigingIntervalType = getVolgendeUitnodigingIntervalType(dossier);
		if (volgendeUitnodigingIntervalType == null)
		{
			LOG.warn("Terecht Positief voor client {} die geen uitnodigingsinterval voor suspect of hoog-risico heeft.", dossier.getClient().getId());
			var peildatum = corrigeerInitielePeildatumVoorNieuwereUitnodiging(dossier, laatsteOnderzoeksDatumVanRonde(screeningRonde));
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT, peildatum);
		}
		else if (volgendeUitnodigingIntervalType == MammaUitnodigingsintervalType.SUSPECT)
		{
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT, dossier.getVolgendeUitnodiging().getPeildatum());
		}
	}

	@Override
	public void updateVolgendeUitnodigingNaDeelname(MammaDossier dossier)
	{
		verlaatSuspectDoorDeelnameOfFoutPositief(dossier);
	}

	private void verlaatSuspectDoorDeelnameOfFoutPositief(MammaDossier dossier)
	{
		var volgendeUitnodigingIntervalType = getVolgendeUitnodigingIntervalType(dossier);
		if (volgendeUitnodigingIntervalType == MammaUitnodigingsintervalType.SUSPECT)
		{
			verwijderVolgendeUitnodiging(dossier);
		}
		else if (volgendeUitnodigingIntervalType == MammaUitnodigingsintervalType.HOOG_RISICO_SUSPECT)
		{
			zetVolgendeUitnodigingsinterval(dossier, MammaUitnodigingsintervalType.HOOG_RISICO_DEELNAME, dossier.getVolgendeUitnodiging().getPeildatum());
		}
	}

	private void verwijderVolgendeUitnodiging(MammaDossier dossier)
	{
		var volgendeUitnodiging = dossier.getVolgendeUitnodiging();
		dossier.setVolgendeUitnodiging(null);
		hibernateService.delete(volgendeUitnodiging);
		hibernateService.saveOrUpdate(dossier);
	}

	@Override
	public void updateVolgendeUitnodigingBijNieuweUitnodiging(MammaDossier dossier)
	{
		var volgendeUitnodiging = dossier.getVolgendeUitnodiging();
		if (volgendeUitnodiging != null)
		{
			var uitnodingsDatumLaatsteRonde = datumEersteUitnodingsbriefVanLaatsteRonde(dossier);

			if (uitnodingsDatumLaatsteRonde != null && volgendeUitnodiging.getPeildatum().isBefore(uitnodingsDatumLaatsteRonde))
			{
				zetVolgendeUitnodigingsinterval(dossier, volgendeUitnodiging.getInterval().getType(), uitnodingsDatumLaatsteRonde);
			}
		}
	}

	private LocalDate datumEersteUitnodingsbriefVanLaatsteRonde(MammaDossier dossier)
	{
		return dossier.getLaatsteScreeningRonde().getUitnodigingen().stream()
			.filter(u -> u.getBrief() != null) 
			.map(Uitnodiging::getCreatieDatum)
			.min(Comparator.naturalOrder())
			.map(DateUtil::toLocalDate)
			.orElse(null);
	}

	@Override
	public void updateIntervalReferentieDatums()
	{
		for (var type : MammaUitnodigingsintervalType.values())
		{
			var interval = getIntervalByType(type);
			var vandaag = dateSupplier.getLocalDate();
			var referentieDatum = vandaag.minusMonths(interval.getAantalMaanden());
			interval.setBerekendeReferentieDatum(referentieDatum);
			hibernateService.saveOrUpdate(interval);
		}
	}

	private MammaUitnodigingsinterval getIntervalByType(MammaUitnodigingsintervalType interval)
	{
		Map<String, Object> parameters = new HashMap<>();
		parameters.put("type", interval);
		var intervalParameters = hibernateService.getByParameters(MammaUitnodigingsinterval.class, parameters);

		if (intervalParameters.isEmpty())
		{
			throw new IllegalStateException("Kan interval entiteit behorende bij " + interval + " niet vinden");
		}
		return intervalParameters.get(0);
	}
}
