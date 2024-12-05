package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.MammaBeeldenVerwijderenWriter;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.repository.mamma.MammaScreeningRondeRepository;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftBeeldenMetGunstigeUitslag;

@Component
@AllArgsConstructor
public class MammaBeeldenGunstigWriter extends MammaBeeldenVerwijderenWriter<MammaDossier>
{
	public static final Long MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN = 3L;

	private final MammaScreeningRondeRepository screeningRondeRepository;

	@Override
	protected void write(MammaDossier mammaDossier)
	{
		var specification = heeftBeeldenMetGunstigeUitslag(mammaDossier);
		var sort = Sort.by(Sort.Order.desc(ScreeningRonde_.CREATIE_DATUM));
		var rondes = screeningRondeRepository.findWith(specification, q -> q.sortBy(sort)).all(MINIMUM_AANTAL_GUNSTIGE_AFGESLOTEN_RONDES_MET_BEELDEN, -1);

		rondes.forEach(this::verwijderenBeeldenVanScreeningRonde);

	}
}
