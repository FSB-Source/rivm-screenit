package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.batch.service.RevisionInformationService;
import nl.rivm.screenit.model.envers.RevisionInformationResolver;
import nl.rivm.screenit.model.envers.RevisionKenmerk;

import org.springframework.stereotype.Service;

@Service
public class RevisionInformationServiceImpl implements RevisionInformationService
{
	private Map<String, Map<Long, RevisionInformationResolver.RevisionInformationResolverDelegate>> delegates = new ConcurrentHashMap<>();

	@PostConstruct
	public void init()
	{
		RevisionInformationResolver.registerDelegate(new RevisionInformationResolver.RevisionInformationResolverDelegate()
		{
			@Override
			public RevisionKenmerk getRevisionKenmerk()
			{
				for (Map<Long, RevisionInformationResolver.RevisionInformationResolverDelegate> delegete : delegates.values())
				{
					RevisionKenmerk kenmerk = delegete.entrySet().stream()
						.filter(e -> e.getKey().equals(Thread.currentThread().getId()))
						.map(e -> e.getValue().getRevisionKenmerk())
						.findFirst()
						.orElse(null);
					if (kenmerk != null)
					{
						return kenmerk;
					}
				}
				return null;
			}
		});
	}

	@Override
	public void registerKenmerk(String context, RevisionKenmerk revisionKenmerk)
	{
		delegates.computeIfAbsent(context, k -> new ConcurrentHashMap<>());
		delegates.get(context).put(Thread.currentThread().getId(), new RevisionInformationResolver.RevisionInformationResolverDelegate()
		{
			@Override
			public RevisionKenmerk getRevisionKenmerk()
			{
				return revisionKenmerk;
			}
		});
	}

	@Override
	public void unregister(String context)
	{
		delegates.remove(context);
	}
}
