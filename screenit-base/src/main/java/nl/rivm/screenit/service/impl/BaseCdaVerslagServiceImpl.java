package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.repository.algemeen.OntvangenCdaBerichtRepository;
import nl.rivm.screenit.repository.algemeen.VerslagRepository;
import nl.rivm.screenit.service.BaseCdaVerslagService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftBerichtGeenStatusFoutOfWaarschuwing;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftBerichtId;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftBerichtStatus;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftBerichtStatusVerwerkingOfVerwerkt;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftBerichtTypeVoorBvo;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftSetId;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.heeftVersieDieGroterIsDan;

@Service
public class BaseCdaVerslagServiceImpl implements BaseCdaVerslagService
{
	@Autowired
	private OntvangenCdaBerichtRepository ontvangenCdaBerichtRepository;

	@Autowired
	private VerslagRepository verslagRepository;

	@Override
	public boolean isBerichtReedsVerwerkt(String berichtId)
	{
		var spec = heeftBerichtId(berichtId).and(heeftBerichtStatusVerwerkingOfVerwerkt());
		return ontvangenCdaBerichtRepository.exists(spec);
	}

	@Override
	public boolean isBerichtReedsOntvangen(String setId, Long versie)
	{
		var spec = heeftSetId(setId).and(heeftVersieDieGroterIsDan(versie)).and(heeftBerichtGeenStatusFoutOfWaarschuwing());
		return ontvangenCdaBerichtRepository.exists(spec);
	}

	@Override
	public Verslag getVerslag(String setId, Class<? extends Verslag<?, ?>> clazz)
	{
		return verslagRepository.getVerslagVoorSetId(setId, clazz).orElse(null);
	}

	@Override
	public List<Long> getAlleNietVerwerkteCdaBerichten(Bevolkingsonderzoek bvo)
	{
		var specification = heeftBerichtStatus(BerichtStatus.VERWERKING).and(heeftBerichtTypeVoorBvo(bvo));

		return ontvangenCdaBerichtRepository.findWith(specification, Long.class, q -> q
			.projection((cb, r) -> r.get(OntvangenCdaBericht_.id))
			.all());
	}
}
