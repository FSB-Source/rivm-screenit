package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.repository.cervix.CervixZasHoudbaarheidRepository;
import nl.rivm.screenit.repository.colon.ColonFITVervaldatumRepository;
import nl.rivm.screenit.specification.cervix.CervixZasHoudbaarheidSpecification;
import nl.rivm.screenit.specification.colon.ColonFITHoudbaarheidSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftNietId;

@Service
public class HoudbaarheidServiceImpl implements HoudbaarheidService
{
	@Autowired
	private CervixZasHoudbaarheidRepository cervixZasHoudbaarheidRepository;

	@Autowired
	private ColonFITVervaldatumRepository colonFITVervaldatumRepository;

	@Override
	public <H extends AbstractHoudbaarheid> List<H> getHoudbaarheidItems(Class<H> clazz, long first, long count, Sort sort)
	{
		if (clazz.equals(IFOBTVervaldatum.class))
		{
			return (List<H>) colonFITVervaldatumRepository.findWith(null, q -> q.sortBy(sort)).all(first, count);
		}
		else
		{
			return (List<H>) cervixZasHoudbaarheidRepository.findWith(null, q -> q.sortBy(sort)).all(first, count);
		}
	}

	@Override
	public <H extends AbstractHoudbaarheid> Long countHoudbaarheidItems(Class<H> clazz)
	{
		if (clazz.equals(IFOBTVervaldatum.class))
		{
			return colonFITVervaldatumRepository.count();
		}
		else
		{
			return cervixZasHoudbaarheidRepository.count();
		}
	}

	@Override
	public <H extends AbstractHoudbaarheid> boolean overlaptBestaandeReeks(H vervalDatum)
	{
		if (vervalDatum instanceof IFOBTVervaldatum)
		{
			return overlaptBestaandeFitReeks((IFOBTVervaldatum) vervalDatum);
		}
		else
		{
			return overlaptBestaandeZasReeks((CervixZasHoudbaarheid) vervalDatum);
		}
	}

	private boolean overlaptBestaandeFitReeks(IFOBTVervaldatum vervalDatum)
	{
		var barcodeRange = Range.closed(vervalDatum.getBarcodeStart(), vervalDatum.getBarcodeEnd());
		var specification = ColonFITHoudbaarheidSpecification.overlaptBarcode(barcodeRange)
			.and(ColonFITHoudbaarheidSpecification.heeftBarcodeLengte(vervalDatum.getBarcodeStart().length()));
		if (vervalDatum.getId() != null)
		{
			specification = specification.and(heeftNietId(vervalDatum.getId()));
		}
		return colonFITVervaldatumRepository.exists(specification);
	}

	private boolean overlaptBestaandeZasReeks(CervixZasHoudbaarheid houdbaarheid)
	{
		var barcodeRange = Range.closed(houdbaarheid.getBarcodeStart(), houdbaarheid.getBarcodeEnd());
		var specification = CervixZasHoudbaarheidSpecification.overlaptBarcode(barcodeRange)
			.and(CervixZasHoudbaarheidSpecification.heeftBarcodeLengte(houdbaarheid.getBarcodeStart().length()));
		if (houdbaarheid.getId() != null)
		{
			specification = specification.and(heeftNietId(houdbaarheid.getId()));
		}
		return cervixZasHoudbaarheidRepository.exists(specification);
	}
}
