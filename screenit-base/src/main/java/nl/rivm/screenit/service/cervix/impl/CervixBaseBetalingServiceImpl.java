package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief_;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.repository.cervix.CervixBoekRegelRepository;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseBetalingService;
import nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class CervixBaseBetalingServiceImpl implements CervixBaseBetalingService
{
	private final CervixBoekRegelRepository boekRegelRepository;

	private final HibernateService hibernateService;

	private final Cervix2023StartBepalingService cervix2023StartBepalingService;

	@Override
	public long totaalAantalHuisartsBoekRegelsInBetaalopdracht(CervixBetaalopdracht betaalopdracht, boolean debet)
	{
		return boekRegelRepository
			.count(CervixBoekRegelSpecification.metOpdrachtID(betaalopdracht.getId())
				.and(CervixBoekRegelSpecification.isHuisartsBetaalopdrachtRegel())
				.and(CervixBoekRegelSpecification.metDebet(debet)));
	}

	@Override
	public BigDecimal totaalBedragHuisartsBoekRegelsInBetaalopdracht(CervixBetaalopdracht betaalopdracht, boolean debet)
	{
		var entityManager = hibernateService.getHibernateSession();
		var cb = entityManager.getCriteriaBuilder();
		var query = cb.createQuery(BigDecimal.class);
		var root = query.from(CervixBoekRegel.class);
		root.alias("boekRegel");

		query.select(cb.sum(cb.treat(root.get(CervixBoekRegel_.tarief), CervixHuisartsTarief.class).get(CervixHuisartsTarief_.tarief)));
		query.where(CervixBoekRegelSpecification.metOpdrachtID(betaalopdracht.getId())
			.and(CervixBoekRegelSpecification.isHuisartsBetaalopdrachtRegel())
			.and(CervixBoekRegelSpecification.metDebet(debet)).toPredicate(root, query, cb));

		var result = entityManager.createQuery(query).getSingleResult();
		return result != null ? result : BigDecimal.ZERO;
	}

	@Override
	public BigDecimal totaalBedragLaboratoriumBoekRegelsInBetaalopdracht(CervixBetaalopdracht betaalopdracht)
	{
		return boekRegelRepository.totaalBedragLaboratoriumBoekRegels(betaalopdracht.getId());
	}

	@Override
	public BigDecimal totaalBedragInBetaalopdracht(CervixBetaalopdracht betaalopdracht)
	{
		return totaalBedragLaboratoriumBoekRegelsInBetaalopdracht(betaalopdracht).add(totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht));
	}

	@Override
	public BigDecimal totaalBedragHuisartsBoekRegelsInBetaalopdracht(CervixBetaalopdracht betaalopdracht)
	{
		return totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, false).subtract(totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, true));
	}

	@Override
	public String getTariefString(CervixTarief tarief)
	{
		String tariefTekst = "'";

		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			tariefTekst += "huisartstarief " + CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(tarief);
		}
		else
		{
			var bmhk2023Lab = cervix2023StartBepalingService.isBmhk2023Tarief(tarief);

			tariefTekst += CervixTariefType.getAlleLabTariefTypes(bmhk2023Lab).stream()
				.filter(t -> t.getBedragVanTarief(tarief).compareTo(BigDecimal.valueOf(0)) > 0)
				.map(t -> t.getNaam() + ": tarief " + t.getBedragStringVanTarief(tarief))
				.collect(Collectors.joining(", "));
		}
		if (Boolean.TRUE.equals(tarief.getActief()))
		{
			tariefTekst += " " + CervixTariefUtil.getGeldigheidMelding(tarief);
		}
		else
		{
			tariefTekst += " (verwijderd)";
		}
		tariefTekst += "'";
		return tariefTekst;
	}
}
