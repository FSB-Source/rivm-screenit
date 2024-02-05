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

import nl.rivm.screenit.main.dao.KwaliteitscontroleLabDao;
import nl.rivm.screenit.main.service.KwaliteitscontroleLabService;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class KwaliteitscontroleLabServiceImpl implements KwaliteitscontroleLabService
{

	@Autowired
	private KwaliteitscontroleLabDao kwaliteitscontroleDao;

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public SKMLInterneControleSet laagOfHoogSample(Instelling instelling)
	{
		return kwaliteitscontroleDao.laagOfHoogSample(instelling);
	}

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public SKMLInterneControleBarcode getInterneControleBarcode(String barcode)
	{
		return kwaliteitscontroleDao.getInterneControleBarcode(barcode);
	}

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public SKMLExterneControleBarcode getExterneControleBarcode(String barcode)
	{
		return kwaliteitscontroleDao.getExterneControleBarcode(barcode);
	}

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public SKMLSentineelControleBarcode getSentineelControleBarcode(String barcode)
	{
		return kwaliteitscontroleDao.getSentineelControleBarcode(barcode);
	}

	@Override
	public boolean checkOfBarcodeAlBestaat(String barcode)
	{
		return kwaliteitscontroleDao.checkOfBarcodeAlBestaat(barcode);
	}
}
