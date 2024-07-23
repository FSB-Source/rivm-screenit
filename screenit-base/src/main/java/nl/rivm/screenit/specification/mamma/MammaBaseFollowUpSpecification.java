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

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBaseFollowUpSpecification
{

	public static Specification<MammaFollowUpRadiologieVerslag> heeftOpenstaandeRadioVerslagenVanInstelling(Instelling instelling)
	{
		return (r, q, cb) ->
			cb.and(
				cb.isNull(r.get(MammaFollowUpRadiologieVerslag_.ingevoerdDoor)),
				cb.equal(r.get(MammaFollowUpRadiologieVerslag_.aangemaaktIn), instelling));
	}

	public static Specification<MammaFollowUpRadiologieVerslag> filterLaatsteRadioVerslagenOpBeoordelingStatus(MammaBeoordelingStatus beoordelingStatus)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = SpecificationUtil.join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var dossierJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.dossier);
			var laatsteOnderzoekJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var laatsteBeoordelingJoin = SpecificationUtil.join(laatsteOnderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);

			var beoordelingPredicate = cb.equal(laatsteBeoordelingJoin, dossierJoin.get(MammaDossier_.laatsteBeoordelingMetUitslag));

			if (beoordelingStatus == null)
			{
				return beoordelingPredicate;
			}

			return cb.and(
				beoordelingPredicate,
				MammaBeoordelingSpecification.heeftStatusPredicate(beoordelingStatus).withPath(cb, laatsteBeoordelingJoin)
			);
		};
	}
}
