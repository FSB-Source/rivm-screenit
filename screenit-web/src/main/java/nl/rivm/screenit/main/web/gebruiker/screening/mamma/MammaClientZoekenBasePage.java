package nl.rivm.screenit.main.web.gebruiker.screening.mamma;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaClientZoekenBasePage extends MammaScreeningBasePage
{
	private IModel<Client> clientFilter = createClientZoekObjectModel();

	private final Form<Client> clientZoekForm = new Form<>("clientZoekForm", clientFilter);

	@SpringBean
	protected ClientService clientService;

	protected IModel<Client> clientOpt = null;

	@SpringBean
	protected LogService logService;

	protected MammaClientZoekenBasePage()
	{
		createZoekContainer();
	}

	protected MammaClientZoekenBasePage(Client client)
	{
		this();
		if (client != null)
		{
			clientFilter.getObject().getPersoon().setBsn(client.getPersoon().getBsn());
			clientFilter.getObject().getPersoon().setGeboortedatum(client.getPersoon().getGeboortedatum());
		}
	}

	private IModel<Client> createClientZoekObjectModel()
	{
		Client client = new Client();
		GbaPersoon gbaPersoon = new GbaPersoon();
		gbaPersoon.setGbaAdres(new BagAdres());
		client.setPersoon(gbaPersoon);
		return new CompoundPropertyModel<>(client);
	}

	private void createZoekContainer()
	{
		clientZoekForm.setOutputMarkupId(true);
		add(clientZoekForm);

		Component geboortedatumDateField = new ScreenitDateTextField("persoon.geboortedatum")
			.setRequired(true)
			.setOutputMarkupId(true);
		geboortedatumDateField.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(geboortedatumDateField);
			}
		});
		clientZoekForm.add(geboortedatumDateField);
		TextField<String> bsnField = new TextField<>("persoon.bsn");
		bsnField.setRequired(true);
		clientZoekForm.add(bsnField.add(new BSNValidator()));

		IndicatingAjaxSubmitLink submitBtn = new IndicatingAjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				zoekClient(target);
			}
		};
		clientZoekForm.add(submitBtn);
		clientZoekForm.setDefaultButton(submitBtn);
	}

	protected void zoekClient(AjaxRequestTarget target)
	{
		String zoekBsn = clientFilter.getObject().getPersoon().getBsn();
		if (StringUtils.isNotBlank(zoekBsn))
		{
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, ScreenitSession.get().getLoggedInAccount(), "In het uitwisselportaal is gezocht op BSN: " + zoekBsn);
		}
		List<Client> clients = clientService.zoekClienten(clientFilter.getObject());
		if (clients.size() == 1)
		{
			Client client = clients.get(0);
			if (!BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS, Bevolkingsonderzoek.MAMMA) && client.getMammaDossier() != null)
			{
				this.clientOpt = ModelUtil.sModel(client);
				updateContent();
				resetClientFilter(target);
			}
			else
			{
				error("Client niet gevonden");
			}
		}
		else if (clients.isEmpty())
		{
			error("Client niet gevonden");
		}
		else
		{
			throw new IllegalStateException("Te veel clienten gevonden, mag niet voorkomen");
		}
	}

	protected abstract void updateContent();

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientFilter);
		ModelUtil.nullSafeDetach(clientOpt);
	}

	protected void resetClientFilter(AjaxRequestTarget target)
	{
		clientFilter = createClientZoekObjectModel();
		clientZoekForm.setModel(clientFilter);
		target.add(clientZoekForm);
	}
}
