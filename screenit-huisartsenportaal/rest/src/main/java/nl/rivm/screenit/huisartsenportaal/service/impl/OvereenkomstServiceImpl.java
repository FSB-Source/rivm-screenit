package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.io.InputStream;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.huisartsenportaal.dto.OvereenkomstDto;
import nl.rivm.screenit.huisartsenportaal.model.Overeenkomst;
import nl.rivm.screenit.huisartsenportaal.repository.OvereenkomstRepository;
import nl.rivm.screenit.huisartsenportaal.service.OvereenkomstService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.aspose.words.License;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OvereenkomstServiceImpl implements OvereenkomstService
{

	private static final Logger LOG = LoggerFactory.getLogger(OvereenkomstServiceImpl.class);

	@Autowired
	private OvereenkomstRepository overeenkomstRepository;

	@PostConstruct
	public void init()
	{
		try
		{
			License license = new License();
			InputStream stream = getClass().getResourceAsStream("/aspose/Aspose.Words.lic");
			license.setLicense(stream);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Overeenkomst saveOrUpdateOvereenkomst(OvereenkomstDto overeenkomstDto)
	{
		Overeenkomst overeenkomst = overeenkomstRepository.findByScreenitId(overeenkomstDto.getHuisartsportaalId());
		if (overeenkomst == null)
		{
			overeenkomst = new Overeenkomst();
			overeenkomst.setScreenitId(overeenkomstDto.getScreenitId());
		}
		overeenkomst.setLaatsteWijzigDatum(overeenkomstDto.getLaatsteWijzigDatum());
		overeenkomst.setPath(overeenkomstDto.getPath());
		overeenkomst.setFileName(overeenkomstDto.getNaam());

		overeenkomstRepository.save(overeenkomst);

		return overeenkomst;
	}

	@Override
	public Overeenkomst geefLaatsteOvereenkomst()
	{
		return overeenkomstRepository.findFirstByOrderByLaatsteWijzigDatumDesc();
	}

	@Override
	public OvereenkomstDto convertOvereenkomstToDto(Overeenkomst overeenkomst)
	{
		return null;
	}
}
