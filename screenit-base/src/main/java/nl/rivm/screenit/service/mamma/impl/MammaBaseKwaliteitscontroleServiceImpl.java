package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.repository.mamma.MammaAdhocMeekijkverzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaFotobesprekingOnderzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaVisitatieOnderzoekRepository;
import nl.rivm.screenit.service.mamma.MammaBaseKwaliteitscontroleService;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.mamma.MammaKwaliteitscontroleSpecification.adhocMeekijkverzoekHeeftScreeningRonde;
import static nl.rivm.screenit.specification.mamma.MammaKwaliteitscontroleSpecification.fotoBesprekingHeeftScreeningRonde;
import static nl.rivm.screenit.specification.mamma.MammaKwaliteitscontroleSpecification.visitatieHeeftScreeningRonde;

@Service
public class MammaBaseKwaliteitscontroleServiceImpl implements MammaBaseKwaliteitscontroleService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaFotobesprekingOnderzoekRepository fotobesprekingOnderzoekRepository;

	@Autowired
	private MammaVisitatieOnderzoekRepository visitatieOnderzoekRepository;

	@Autowired
	private MammaAdhocMeekijkverzoekRepository adhocMeekijkverzoekRepository;

	@Override
	public List<MammaFotobesprekingOnderzoek> getFotobesprekingOnderzoeken(MammaScreeningRonde screeningRonde)
	{
		return fotobesprekingOnderzoekRepository.findAll(fotoBesprekingHeeftScreeningRonde(screeningRonde));
	}

	@Override
	@Transactional
	public void verwijderKwaliteitscontroleOnderzoeken(MammaScreeningRonde screeningRonde)
	{
		visitatieOnderzoekRepository.findAll(visitatieHeeftScreeningRonde(screeningRonde)).forEach(this::verwijderVisitatieOnderzoek);
		adhocMeekijkverzoekRepository.findAll(adhocMeekijkverzoekHeeftScreeningRonde(screeningRonde)).forEach(this::verwijderAdhocMeekijkverzoek);
		fotobesprekingOnderzoekRepository.findAll(fotoBesprekingHeeftScreeningRonde(screeningRonde)).forEach(this::verwijderFotobesprekingOnderzoek);
	}

	private void verwijderVisitatieOnderzoek(MammaVisitatieOnderzoek onderzoek)
	{
		var visitatie = onderzoek.getVisitatie();
		visitatie.getOnderzoeken().remove(onderzoek);
		hibernateService.delete(onderzoek);
		hibernateService.saveOrUpdate(visitatie);
	}

	private void verwijderAdhocMeekijkverzoek(MammaAdhocMeekijkverzoek verzoek)
	{
		var onderzoek = verzoek.getOnderzoek();
		onderzoek.setMeekijkverzoek(null);
		hibernateService.delete(verzoek);
		hibernateService.saveOrUpdate(onderzoek);
	}

	private void verwijderFotobesprekingOnderzoek(MammaFotobesprekingOnderzoek onderzoek)
	{
		var fotobespreking = onderzoek.getFotobespreking();
		fotobespreking.getOnderzoeken().remove(onderzoek);
		hibernateService.delete(onderzoek);
		hibernateService.saveOrUpdate(fotobespreking);
	}

	@Override
	public Long getNextAdhocMeekrijkverzoekVolgnummer()
	{
		return hibernateService.getHibernateSession()
			.doReturningWork(new SequenceGenerator(DatabaseSequence.MEEKIJKVERZOEK_ID, hibernateService.getHibernateSession().getSessionFactory()));
	}

}
