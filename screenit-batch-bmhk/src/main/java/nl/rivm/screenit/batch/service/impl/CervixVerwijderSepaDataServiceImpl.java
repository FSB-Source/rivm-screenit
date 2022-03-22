package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.service.CervixVerwijderSepaDataService;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.repository.CervixBoekRegelRepository;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixVerwijderSepaDataServiceImpl implements CervixVerwijderSepaDataService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixBoekRegelRepository cervixBoekRegelRepository;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	public List<CervixBoekRegel> haalBoekRegelsOp(Long id, int aantal)
	{
		Pageable limit = PageRequest.of(0, aantal);
		return cervixBoekRegelRepository
			.findAll(cervixBoekRegelRepository.baseSpecification()
				.and(cervixBoekRegelRepository.metOpdrachtID(id))
				.and(cervixBoekRegelRepository.metSpecificatie()), limit)
			.getContent();
	}

	@Override
	public long aantalTeVerwerkenBoekregels(Long id)
	{
		return cervixBoekRegelRepository
			.count(cervixBoekRegelRepository.baseSpecification()
				.and(cervixBoekRegelRepository.metOpdrachtID(id)));
	}

	@Override
	public CervixBetaalopdracht haalBetaalOpdrachtOp(Long id)
	{
		return hibernateService.get(CervixBetaalopdracht.class, id);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void ontkoppelBoekregelsVanSpecificatie(List<CervixBoekRegel> boekRegels)
	{
		boekRegels.forEach(boekRegel -> {
			boekRegel.setSpecificatie(null);
			hibernateService.saveOrUpdate(boekRegel);
			if (boekRegel.getVerrichting() != null &&
				boekRegel.getVerrichting().getType() == CervixTariefType.HUISARTS_UITSTRIJKJE)
			{
				huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(boekRegel.getVerrichting()));
			}
		});
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderBetaalopdracht(CervixBetaalopdracht betaalopdracht)
	{
		UploadDocument sepaDocument = betaalopdracht.getSepaDocument();
		UploadDocument sepaSpecificatiePdf = betaalopdracht.getSepaSpecificatiePdf();
		betaalopdracht.setSepaDocument(null);
		betaalopdracht.setSepaSpecificatiePdf(null);
		if (sepaDocument != null)
		{
			uploadDocumentService.delete(sepaDocument, true);
		}
		if (sepaSpecificatiePdf != null)
		{
			uploadDocumentService.delete(sepaSpecificatiePdf, true);
		}
		hibernateService.delete(betaalopdracht);
	}
}
