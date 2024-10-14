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

import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingDao;
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

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.ceProcesMonitoringSpecification;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.ceWerklijstSpecification;
import static nl.rivm.screenit.util.DateUtil.minusWerkdagen;

@Service
public class MammaCeWerklijstServiceImpl implements MammaCeWerklijstService
{
	@Autowired
	private MammaBeoordelingDao beoordelingDao;

	@Autowired
	private MammaOnderzoekRepository onderzoekRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSuplier;

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
		return currentDateSuplier.getLocalDate().minusMonths(1);
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
		var peildatumProcesMonitoring = minusWerkdagen(currentDateSuplier.getLocalDate(), 2);
		return ceProcesMonitoringSpecification(zoekObject, peildatumProcesMonitoring, getPeildatumOngunstigeUitslagen());
	}

	@Override
	public List<MammaBeoordeling> zoekFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return beoordelingDao.zoekFollowUpNietGedownloadBeoordelingen(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public long countFollowUpBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		return beoordelingDao.countFollowUpNietGedownloadBeoordelingen(zoekObject);
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
