package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;

import org.springframework.batch.item.ItemReader;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixVerlateDeelnameCovidReader implements ItemReader<Long>
{
	@Override
	public Long read()
	{
		throw new UnsupportedOperationException("Deze batch wordt niet meer ondersteund");
	}
}
