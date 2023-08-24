package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.agenda.ColonAfspraakPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class ColonClientContactAfspraakWijzigenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private ColonAfspraakPanel afspraken;

	private Panel container;

	public ColonClientContactAfspraakWijzigenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		afspraken = new ColonAfspraakPanel("afspraken", client)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void afspraakWijzigen(AjaxRequestTarget target, Afspraak afspraak, boolean locatieWijzigen)
			{
				ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				ColonClientAfspraakVerplaatsenPanel afspraakVerplaatsen = new ColonClientAfspraakVerplaatsenPanel(container.getId(), ModelUtil.cModel(intakeAfspraak),
					locatieWijzigen);
				afspraakVerplaatsen.setOutputMarkupId(true);
				container.replaceWith(afspraakVerplaatsen);
				container = afspraakVerplaatsen;
				target.add(afspraakVerplaatsen);
			}

			@Override
			public void afspraakAfzeggen(AjaxRequestTarget target, Afspraak afspraak)
			{
				ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
				ColonClientAfspraakAfzeggenPanel afspraakAfzeggen = new ColonClientAfspraakAfzeggenPanel(container.getId(), ModelUtil.cModel(intakeAfspraak));
				afspraakAfzeggen.setOutputMarkupId(true);
				container.replaceWith(afspraakAfzeggen);
				container = afspraakAfzeggen;
				target.add(afspraakAfzeggen);
			}

		};
		afspraken.setOutputMarkupId(true);
		add(afspraken);

		Afspraak afspraak = (Afspraak) extraPanelParams.stream().filter(p -> p instanceof Afspraak).findFirst().orElse(null);
		if (afspraak != null)
		{
			AfspraakStatus status = (AfspraakStatus) extraPanelParams.stream().filter(p -> p instanceof AfspraakStatus).findFirst().orElse(null);
			if (AfspraakStatus.VERPLAATST.equals(status))
			{
				Boolean loactieWijzigen = (Boolean) extraPanelParams.stream().filter(p -> p instanceof Boolean).findFirst().orElse(null);
				container = new ColonClientAfspraakVerplaatsenPanel("container", ModelUtil.cModel((ColonIntakeAfspraak) afspraak), loactieWijzigen);
			}
			else
			{
				container = new ColonClientAfspraakAfzeggenPanel("container", ModelUtil.cModel((ColonIntakeAfspraak) afspraak));
			}
		}
		else
		{
			container = new EmptyPanel("container");
		}
		container.setOutputMarkupId(true);
		add(container);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		if (container instanceof ColonClientAfspraakVerplaatsenPanel)
		{
			return ((ColonClientAfspraakVerplaatsenPanel) container).getOpslaanObjecten();
		}
		if (container instanceof ColonClientAfspraakAfzeggenPanel)
		{
			return ((ColonClientAfspraakAfzeggenPanel) container).getOpslaanObjecten();
		}
		return super.getOpslaanObjecten();
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		if (container instanceof ColonClientAfspraakVerplaatsenPanel)
		{
			return ((ColonClientAfspraakVerplaatsenPanel) container).getOpslaanMeldingen();
		}
		if (container instanceof ColonClientAfspraakAfzeggenPanel)
		{
			return ((ColonClientAfspraakAfzeggenPanel) container).getOpslaanMeldingen();
		}
		return super.getOpslaanMeldingen();
	}

	@Override
	public void validate()
	{
		super.validate();
		if (container instanceof ColonClientAfspraakVerplaatsenPanel)
		{
			((ColonClientAfspraakVerplaatsenPanel) container).validate();
		}
		else if (container instanceof EmptyPanel)
		{
			error(getString("error.clientafspraakwijzigen.geenwijziging"));
		}
	}
}
