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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.colon.SKMLExternSchema_;
import nl.rivm.screenit.repository.colon.ColonSKMLExternSchemaRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLExterneControleBarcodeRepository;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.colon.ColonSKMLControleBarcodeSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.filterActief;
import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.filterDeadline;
import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.filterJaar;
import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.filterLetter;
import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.filterRonde;
import static nl.rivm.screenit.specification.colon.ColonSKMLExternSchemaSpecification.heeftDeadlineVanaf;

@Service
public class SKMLExternSchemaServiceImpl implements SKMLExternSchemaService
{
	@Autowired
	private ColonSKMLExternSchemaRepository skmlExternSchemaRepository;

	@Autowired
	private ColonSKMLExterneControleBarcodeRepository skmlExterneControleBarcodeRepository;

	@Override
	public List<SKMLExternSchema> zoekSchemas(SKMLExternSchema zoekObject, Sort sort, int first, int count)
	{
		return skmlExternSchemaRepository.findWith(getSpecification(zoekObject), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countSchemas(SKMLExternSchema zoekObject)
	{
		return skmlExternSchemaRepository.countDistinct(getSpecification(zoekObject));
	}

	private Specification<SKMLExternSchema> getSpecification(SKMLExternSchema zoekObject)
	{
		return filterRonde(zoekObject.getRonde())
			.and(filterLetter(zoekObject.getLetter()))
			.and(filterJaar(zoekObject.getJaar()))
			.and(filterDeadline(zoekObject.getDeadline()))
			.and(HibernateObjectSpecification.filterId(zoekObject.getId()))
			.and(filterActief(zoekObject.getActief()));
	}

	@Override
	public long telAantalGekoppeldeBarcodes(SKMLExternSchema zoekObject)
	{
		return skmlExterneControleBarcodeRepository.countDistinct(ColonSKMLControleBarcodeSpecification.heeftSchemaId(zoekObject.getId()));
	}

	@Override
	public boolean magSchemaVerwijderdWorden(SKMLExternSchema zoekObject)
	{
		return telAantalGekoppeldeBarcodes(zoekObject) == 0;
	}

	@Override
	public SKMLExternSchema haalEerstvolgendeSchemaOp(Date deadline)
	{
		return skmlExternSchemaRepository.findFirst(getSchemaVanafDeadlineSpecification(deadline), Sort.by(SKMLExternSchema_.DEADLINE)).orElse(null);
	}

	@Override
	public List<SKMLExternSchema> zoekSchema(SKMLExternSchema zoekObject)
	{
		return skmlExternSchemaRepository.findAll(filterActief(true).and(filterDeadline(zoekObject.getDeadline())));
	}

	@Override
	public List<SKMLExternSchema> haalSchemasVanafDeadlineDatum(Date deadline)
	{
		return skmlExternSchemaRepository.findAll(getSchemaVanafDeadlineSpecification(deadline));
	}

	private static ExtendedSpecification<SKMLExternSchema> getSchemaVanafDeadlineSpecification(Date deadline)
	{
		return filterActief(true).and(heeftDeadlineVanaf(deadline));
	}

}
