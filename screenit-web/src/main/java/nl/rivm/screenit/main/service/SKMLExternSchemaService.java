package nl.rivm.screenit.main.service;

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

import nl.rivm.screenit.model.colon.SKMLExternSchema;

import org.springframework.data.domain.Sort;

public interface SKMLExternSchemaService
{
	List<SKMLExternSchema> zoekSchemas(SKMLExternSchema zoekObject, Sort sort, int first, int count);

	long countSchemas(SKMLExternSchema zoekObject);

	long telAantalGekoppeldeBarcodes(SKMLExternSchema zoekObject);

	boolean magSchemaVerwijderdWorden(SKMLExternSchema zoekObject);

	SKMLExternSchema haalEerstvolgendeSchemaOp(Date deadline);

	List<SKMLExternSchema> zoekSchema(SKMLExternSchema zoekObject);

	List<SKMLExternSchema> haalSchemasVanafDeadlineDatum(Date deadline);
}
