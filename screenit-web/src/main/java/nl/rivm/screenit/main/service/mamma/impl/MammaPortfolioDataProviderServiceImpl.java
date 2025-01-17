package nl.rivm.screenit.main.service.mamma.impl;

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
import java.util.stream.Collectors;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaPortfolioZoekObject;
import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftOnderzoekMetBeeldenGemaaktDoor;

@Service("mammaPortfolioDataProviderService")
public class MammaPortfolioDataProviderServiceImpl extends RepositoryDataProviderService<Client, ClientRepository, MammaPortfolioZoekObject>
{
	@Override
	protected Specification<Client> getSpecification(MammaPortfolioZoekObject zoekObject, Sort sortParam)
	{
		var instellingGebruikers = getInstellingGebruikersVoorGebruikers(zoekObject.getGebruikers());
		var range = Range.closed(DateUtil.toLocalDate(zoekObject.getVanaf()), DateUtil.toLocalDate(zoekObject.getTotEnMet()));
		return heeftOnderzoekMetBeeldenGemaaktDoor(instellingGebruikers, range);
	}

	private List<InstellingGebruiker> getInstellingGebruikersVoorGebruikers(List<Gebruiker> gebruikers)
	{
		return gebruikers.stream().flatMap(gebruiker -> gebruiker.getOrganisatieMedewerkers().stream()).collect(Collectors.toList());
	}

	public List<Long> zoekPortfolioClientenIds(MammaPortfolioZoekObject zoekObject, Sort sortParam)
	{
		return getRepository().findWith(getSpecification(zoekObject, sortParam), Long.class, q ->
			q.sortBy(sortParam)
				.projection((cb, r) -> r.get(Client_.id))
				.all());
	}
}
