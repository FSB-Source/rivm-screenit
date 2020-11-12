package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.mamma.MammaBaseKwaliteitscontroleDao;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.service.mamma.MammaBaseKwaliteitscontroleService;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseKwaliteitscontroleServiceImpl implements MammaBaseKwaliteitscontroleService
{
	@Autowired
	private MammaBaseKwaliteitscontroleDao kwaliteitscontroleDao;

	@Autowired
	private HibernateService hibernateService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderKwaliteitscontroleOnderzoeken(MammaDossier dossier)
	{
		List<MammaFotobesprekingOnderzoek> fbOnderzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(dossier, MammaFotobesprekingOnderzoek.class);

		verwijderFotobesprekingOnderzoeken(fbOnderzoeken);

		List<MammaVisitatieOnderzoek> vOnderzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(dossier, MammaVisitatieOnderzoek.class);

		verwijderVisitatieOnderzoeken(vOnderzoeken);

		List<MammaAdhocMeekijkverzoek> verzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(dossier, MammaAdhocMeekijkverzoek.class);

		verwijderAdhocMeekijkverzoeken(verzoeken);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderKwaliteitscontroleOnderzoeken(MammaScreeningRonde screeningRonde)
	{
		List<MammaFotobesprekingOnderzoek> fbOnderzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(screeningRonde, MammaFotobesprekingOnderzoek.class);

		verwijderFotobesprekingOnderzoeken(fbOnderzoeken);

		List<MammaVisitatieOnderzoek> vOnderzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(screeningRonde, MammaVisitatieOnderzoek.class);

		verwijderVisitatieOnderzoeken(vOnderzoeken);

		List<MammaAdhocMeekijkverzoek> verzoeken = kwaliteitscontroleDao.getKwaliteitscontroleOnderzoeken(screeningRonde, MammaAdhocMeekijkverzoek.class);

		verwijderAdhocMeekijkverzoeken(verzoeken);
	}

	private void verwijderVisitatieOnderzoeken(List<MammaVisitatieOnderzoek> vOnderzoeken)
	{
		for (MammaVisitatieOnderzoek onderzoek : vOnderzoeken)
		{
			MammaVisitatie visitatie = onderzoek.getVisitatie();
			visitatie.getOnderzoeken().remove(onderzoek);
			hibernateService.delete(onderzoek);
			hibernateService.saveOrUpdate(visitatie);
		}
	}

	private void verwijderAdhocMeekijkverzoeken(List<MammaAdhocMeekijkverzoek> verzoeken)
	{
		for (MammaAdhocMeekijkverzoek verzoek : verzoeken)
		{
			MammaOnderzoek onderzoek = verzoek.getOnderzoek();
			onderzoek.setMeekijkverzoek(null);
			hibernateService.delete(verzoek);
			hibernateService.saveOrUpdate(onderzoek);
		}
	}

	private void verwijderFotobesprekingOnderzoeken(List<MammaFotobesprekingOnderzoek> fbOnderzoeken)
	{
		for (MammaFotobesprekingOnderzoek onderzoek : fbOnderzoeken)
		{
			MammaFotobespreking fotobespreking = onderzoek.getFotobespreking();
			fotobespreking.getOnderzoeken().remove(onderzoek);
			hibernateService.delete(onderzoek);
			hibernateService.saveOrUpdate(fotobespreking);
		}
	}

	@Override
	public Long getNextAdhocMeekrijkverzoekVolgnummer()
	{
		return hibernateService.getHibernateSession()
			.doReturningWork(new SequenceGenerator(DatabaseSequence.MEEKIJKVERZOEK_ID, hibernateService.getHibernateSession().getSessionFactory()));
	}

}
