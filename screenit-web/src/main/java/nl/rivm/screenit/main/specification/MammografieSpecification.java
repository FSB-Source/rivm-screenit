package nl.rivm.screenit.main.specification;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammografieSpecification
{
	public static Specification<Client> heeftBeeldenBeschikbaar()
	{
		return (r, q, cb) ->
		{
			var mammografie = r.join(Client_.mammaDossier).join(MammaDossier_.screeningRondes).join(MammaScreeningRonde_.uitnodigingen).join(MammaUitnodiging_.afspraken)
				.join(MammaAfspraak_.onderzoek).join(MammaOnderzoek_.mammografie);
			q.distinct(true);

			return cb.in(mammografie.get(MammaMammografie_.ILM_STATUS))
				.value(MammaMammografieIlmStatus.BEELDEN_MOGELIJK_AANWEZIG);
		};
	}
}
