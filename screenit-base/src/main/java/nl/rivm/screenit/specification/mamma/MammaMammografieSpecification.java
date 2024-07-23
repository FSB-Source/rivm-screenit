package nl.rivm.screenit.specification.mamma;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaMammografieSpecification
{
	public static Specification<Client> heeftClientLaatsteOnderzoekMetDensiteit(MammaDenseWaarde densiteit)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = SpecificationUtil.join(r, Client_.mammaDossier);
			var rondeJoin = SpecificationUtil.join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
			var onderzoekJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var mammografieJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.mammografie);
			return cb.equal(mammografieJoin.get(MammaMammografie_.densiteit), densiteit);
		};
	}

	public static Specification<MammaMammografie> heeftUitnodigingsNummer(long uitnodigingsNummer)
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = SpecificationUtil.join(r, MammaMammografie_.onderzoek);
			var afspraakJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.uitnodiging);
			var rondeJoin = SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
			return cb.equal(rondeJoin.get(MammaScreeningRonde_.uitnodigingsNr), uitnodigingsNummer);
		};
	}

	public static Specification<MammaMammografie> heeftIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaMammografie_.ilmStatus), ilmStatus);
	}
}
