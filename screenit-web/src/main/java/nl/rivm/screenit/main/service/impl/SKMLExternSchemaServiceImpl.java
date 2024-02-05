package nl.rivm.screenit.main.service.impl;

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
import java.util.Iterator;

import nl.rivm.screenit.main.dao.SKMLExternSchemaDao;
import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.model.colon.SKMLExternSchema;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class SKMLExternSchemaServiceImpl implements SKMLExternSchemaService
{

	@Autowired
	private SKMLExternSchemaDao schemaDao;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Iterator<? extends SKMLExternSchema> zoekSchemas(SKMLExternSchema zoekObject, String sortProperty, boolean ascending, int first, int count)
	{
		return schemaDao.zoekSchemas(zoekObject, sortProperty, ascending, first, count);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public long telSchemas(SKMLExternSchema zoekObject)
	{
		return schemaDao.telSchemas(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public int telAantalGekoppeldeBarcodes(SKMLExternSchema zoekObject)
	{
		return schemaDao.telAantalGekoppeldeBarcodes(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public boolean magSchemaVerwijderdWorden(SKMLExternSchema zoekObject)
	{
		boolean mag = false;
		if (telAantalGekoppeldeBarcodes(zoekObject) <= 0)
		{
			mag = true;
		}
		return mag;
	}

	@Override
	public SKMLExternSchema haalEerstvolgendeSchemaOp(Date deadline)
	{
		return schemaDao.haalEerstvolgendeSchemaOp(deadline);
	}
}
