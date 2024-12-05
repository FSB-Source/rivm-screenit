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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaCeWerklijstService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.ceProcesMonitoringSpecification;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.ceWerklijstSpecification;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.heeftGeenAfspraakNaScreeningRondeVanBeoordeling;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.heeftGeenPathologieVerslag;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.zijnBeeldenNietGedownload;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatusDatumOpOfVoor;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isVanLaatsteAfspraakVanRonde;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.screeningRondeJoin;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftGeenFollowUpConclusie;
import static nl.rivm.screenit.util.DateUtil.minusWerkdagen;

@Service
public class MammaCeWerklijstServiceImpl implements MammaCeWerklijstService
{
	@Autowired
	private MammaOnderzoekRepository onderzoekRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public List<MammaBeoordeling> zoekBeoordelingen(MammaCeWerklijstZoekObject zoekObject, long first, long count, Sort sort)
	{
		return onderzoekRepository.findWith(getCeWerklijstSpecification(zoekObject), MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.laatsteBeoordeling))
				.sortBy(werklijstSortering(sort))
				.all(first, count));
	}

	private Specification<MammaOnderzoek> getCeWerklijstSpecification(MammaCeWerklijstZoekObject zoekObject)
	{
		return ceWerklijstSpecification(zoekObject, getPeildatumOngunstigeUitslagen());
	}

	private LocalDate getPeildatumOngunstigeUitslagen()
	{
		return currentDateSupplier.getLocalDate().minusMonths(1);
	}

	private Sort werklijstSortering(Sort sort)
	{
		return sort.and(Sort.by(MammaOnderzoek_.CREATIE_DATUM)); 
	}

	@Override
	public long countBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return onderzoekRepository.count(getCeWerklijstSpecification(zoekObject));
	}

	@Override
	public List<MammaBeoordeling> zoekProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject, long first, long count, Sort sort)
	{
		return onderzoekRepository.findWith(processMonitoringSpecification(zoekObject), MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.laatsteBeoordeling))
				.sortBy(werklijstSortering(sort))
				.all(first, count));
	}

	@Override
	public long countProcessmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return onderzoekRepository.count(processMonitoringSpecification(zoekObject));
	}

	private Specification<MammaOnderzoek> processMonitoringSpecification(MammaCeWerklijstZoekObject zoekObject)
	{
		var peildatumProcesMonitoring = minusWerkdagen(currentDateSupplier.getLocalDate(), 2);
		return ceProcesMonitoringSpecification(zoekObject, peildatumProcesMonitoring, getPeildatumOngunstigeUitslagen());
	}

	@Override
	public List<MammaBeoordeling> zoekFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject, long first, long count, Sort sort)
	{
		return onderzoekRepository.findWith(followUpNietGedownloadSpecification(zoekObject), MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.laatsteBeoordeling))
				.sortBy(werklijstSortering(sort))
				.all(first, count));
	}

	@Override
	public long countFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return onderzoekRepository.count(followUpNietGedownloadSpecification(zoekObject));
	}

	private Specification<MammaOnderzoek> followUpNietGedownloadSpecification(MammaCeWerklijstZoekObject zoekObject)
	{
		var peilDatumNietGedownload = currentDateSupplier.getLocalDate()
			.minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST_NA_DAGEN.name(), 42));
		return ceWerklijstSpecification(zoekObject, getPeildatumOngunstigeUitslagen())
			.and(isVanLaatsteAfspraakVanRonde())
			.and(heeftStatus(UITSLAG_ONGUNSTIG).with(MammaOnderzoek_.laatsteBeoordeling))
			.and(heeftStatusDatumOpOfVoor(peilDatumNietGedownload).with(MammaOnderzoek_.laatsteBeoordeling))
			.and(heeftGeenFollowUpConclusie().with(r -> screeningRondeJoin(r)))
			.and(heeftGeenAfspraakNaScreeningRondeVanBeoordeling())
			.and(zijnBeeldenNietGedownload())
			.and(heeftGeenPathologieVerslag());
	}

	@Override
	public List<MammaScreeningsEenheid> zoekScreeningsEenhedenMetCeWerklijstBeoordeling(List<MammaBeoordelingStatus> beschikbareBeoordelingStatussen,
		List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		if (CollectionUtils.isEmpty(beoordelingsEenheden))
		{
			return new ArrayList<>();
		}

		var zoekObject = new MammaCeWerklijstZoekObject();
		zoekObject.setBeoordelingStatussen(beschikbareBeoordelingStatussen);
		zoekObject.setBeoordelingsEenheden(beoordelingsEenheden);

		return onderzoekRepository.findWith(getCeWerklijstSpecification(zoekObject), MammaScreeningsEenheid.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.screeningsEenheid))
				.distinct().all());
	}
}
