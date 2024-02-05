package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.zorginstelling;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GekoppeldePaLabsPanel<T extends Instelling> extends GenericPanel<T>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	public GekoppeldePaLabsPanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		List<Instelling> paLabs = instellingService.getPathologieLabs(getModelObject());
		List<String> instellingsNamen = paLabs.stream().map(Instelling::getNaam).collect(Collectors.toList());
		String paLabsString = !instellingsNamen.isEmpty() ? String.join(", ", instellingsNamen) : "Er zijn geen labs gekoppeld";
		add(new Label("paLabs", paLabsString));
	}
}
