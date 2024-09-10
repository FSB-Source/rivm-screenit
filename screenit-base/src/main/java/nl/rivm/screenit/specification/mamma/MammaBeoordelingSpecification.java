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
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBeoordelingSpecification
{
	public static Specification<Client> heeftClientLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus status)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = SpecificationUtil.join(r, Client_.mammaDossier);
			var rondeJoin = SpecificationUtil.join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
			var onderzoekJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var beoordelingJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
			return cb.equal(beoordelingJoin.get(MammaBeoordeling_.status), status);
		};
	}

	public static PathAwarePredicate<MammaBeoordeling> heeftStatusPredicate(MammaBeoordelingStatus status)
	{
		return (cb, r) -> cb.equal(r.get(MammaBeoordeling_.status), status);
	}

	public static Specification<MammaBeoordeling> isVrijTeGeven(InstellingGebruiker ingelogdeGebruiker) {
		return (r, q, cb) -> cb.and(
			cb.equal(r.get(MammaBeoordeling_.reserveringhouder), ingelogdeGebruiker),
			cb.not(r.get(MammaBeoordeling_.status).in(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)
		));
	}

	public static Specification<MammaBeoordeling> heeftLezing(MammaLezing lezing) {
		return (r, q, cb) -> cb.or(
			cb.equal(r.get(MammaBeoordeling_.eersteLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.tweedeLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.discrepantieLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.arbitrageLezing), lezing),
			cb.equal(r.get(MammaBeoordeling_.verslagLezing), lezing)
		);
	}

	public static Specification<MammaBeoordeling> heeftUitslagStatus()
	{
		return (r, q, cb) -> r.get(MammaBeoordeling_.status).in(MammaBeoordelingStatus.uitslagStatussen());
	}

	public static Specification<MammaBeoordeling> heeftDossier(MammaDossier dossier)
	{
		return (r, q, cb) ->
		{
			var onderzoekJoin = SpecificationUtil.join(r, MammaBeoordeling_.onderzoek);
			var afspraakJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.uitnodiging);
			var screeningRondeJoin = SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
			return cb.equal(screeningRondeJoin.get(MammaScreeningRonde_.dossier), dossier);
		};
	}

}
