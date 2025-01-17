package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.repository.algemeen.BagAdresRepository;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.service.GemeenteService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres_;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.BagAdresSpecification.heeftGbaGemeente;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftGeenBMHKLaboratoriumOfGekoppeldAan;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftGeenScreeningOrganisatieOfGekoppeldAan;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftNaamEnScreeningOrganisatie;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.isGemeenteActiefOpMoment;
import static org.apache.commons.lang.StringUtils.isNotBlank;
import static org.springframework.data.jpa.domain.Specification.where;

@Service
public class GemeenteServiceImpl implements GemeenteService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private GemeenteRepository gemeenteRepository;

	@Autowired
	private BagAdresRepository bagAdresRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional
	public void voegGemeenteToe(Gemeente gemeente)
	{
		var uitnodigingsGebied = new UitnodigingsGebied();
		uitnodigingsGebied.setGemeente(gemeente);
		uitnodigingsGebied.setNaam(gemeente.getNaam());

		gemeenteRepository.save(gemeente);
		hibernateService.saveOrUpdate(uitnodigingsGebied);
	}

	@Override
	public List<Gemeente> getNietOfAanScreeningsOrganisatieGekoppeldGemeentes(ScreeningOrganisatie screeningOrganisatie)
	{
		var nu = currentDateSupplier.getDate();
		var spec = heeftGeenScreeningOrganisatieOfGekoppeldAan(screeningOrganisatie)
			.and(isGemeenteActiefOpMoment(nu));

		return gemeenteRepository.findAll(spec, Sort.by(Gemeente_.NAAM));
	}

	@Override
	public List<Gemeente> getNietOfAanBMHKLaboratoriumGekoppeldGemeentes(BMHKLaboratorium bmhkLaboratorium)
	{
		var nu = currentDateSupplier.getDate();
		var spec = heeftGeenBMHKLaboratoriumOfGekoppeldAan(bmhkLaboratorium)
			.and(isGemeenteActiefOpMoment(nu));

		return gemeenteRepository.findAll(spec, Sort.by(Gemeente_.NAAM));
	}

	@Override
	public List<Gemeente> zoekGemeentes(Gemeente gemeente, long first, long count, String property, boolean ascending)
	{
		var sort = Sort.by(ascending ? Sort.Direction.ASC : Sort.Direction.DESC, property);
		var spec = heeftNaamEnScreeningOrganisatie(gemeente);

		return gemeenteRepository.findWith(spec, q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countGemeentes(Gemeente gemeente)
	{
		var spec = heeftNaamEnScreeningOrganisatie(gemeente);
		return gemeenteRepository.count(spec);
	}

	@Override
	public Boolean getGesplitsOpPostcode(Gemeente gemeente)
	{
		int aantal = 0;
		if (gemeente.getUitnodigingsGebieden() != null)
		{
			aantal = gemeente.getUitnodigingsGebieden().size();
		}
		Boolean gesplitsOpPostcode = null;
		if (aantal > 1)
		{

			for (UitnodigingsGebied gebied : gemeente.getUitnodigingsGebieden())
			{
				if (gebied.getPostcodeGebied() != null)
				{
					gesplitsOpPostcode = Boolean.TRUE;
				}
				else if (isNotBlank(gebied.getWoonplaats()))
				{
					gesplitsOpPostcode = Boolean.FALSE;
				}
				if (gesplitsOpPostcode != null)
				{
					break;
				}
			}
		}
		return gesplitsOpPostcode;
	}

	@Override
	public List<String> getWoonplaatsen(UitnodigingsGebied gebied)
	{
		var spec = where(heeftGbaGemeente(gebied.getGemeente()));

		var woonplaatsen = bagAdresRepository.findWith(spec, String.class, q -> q.projection((cb, r) -> r.get(Adres_.plaats))).all();

		for (var andereGebied : gebied.getGemeente().getUitnodigingsGebieden())
		{
			if (isNotBlank(andereGebied.getWoonplaats()))
			{
				woonplaatsen.remove(andereGebied.getWoonplaats());
			}
		}
		return woonplaatsen;
	}

	@Override
	@Transactional
	public void verwijderAlleGebieden(Gemeente gemeente)
	{
		List<UitnodigingsGebied> toDeleteGebieden = new ArrayList<>();
		for (var gebied : gemeente.getUitnodigingsGebieden())
		{
			if (gebied.getPostcodeGebied() != null || isNotBlank(gebied.getWoonplaats()))
			{
				if (gebied.getVerdeling() != null)
				{
					hibernateService.deleteAll(gebied.getVerdeling());
				}
				toDeleteGebieden.add(gebied);
			}
		}
		for (var toDeleteGebied : toDeleteGebieden)
		{
			gemeente.getUitnodigingsGebieden().remove(toDeleteGebied);
			hibernateService.delete(toDeleteGebied);
		}
		hibernateService.saveOrUpdate(gemeente);
	}

	@Override
	public boolean magAlleGebiedenVerwijderen(Gemeente gemeente)
	{
		for (var gebied : gemeente.getUitnodigingsGebieden())
		{
			if (gebied.getPostcodeGebied() != null || isNotBlank(gebied.getWoonplaats()))
			{
				for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : gebied.getVerdeling())
				{
					if (verdeling.getPercentageAdherentie() != null && verdeling.getPercentageAdherentie() > 0)
					{
						return false;
					}
				}
			}
		}
		return true;
	}

}
