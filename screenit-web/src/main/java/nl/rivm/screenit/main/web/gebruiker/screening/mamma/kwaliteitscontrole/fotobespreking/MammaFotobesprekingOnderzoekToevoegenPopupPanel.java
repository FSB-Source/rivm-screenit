package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaFotobesprekingOnderzoekToevoegenPopupPanel extends GenericPanel<MammaFotobespreking>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private ClientService clientService;

	public MammaFotobesprekingOnderzoekToevoegenPopupPanel(String id, IModel<MammaFotobespreking> fotobesprekingModel)
	{
		super(id, fotobesprekingModel);
		GbaPersoon zoekPersoon = new GbaPersoon();
		Form<GbaPersoon> form = new ScreenitForm<>("form", new CompoundPropertyModel<>(zoekPersoon));
		form.add(new TextField<>("bsn").setRequired(true).setOutputMarkupId(true).add(new BSNValidator()));
		form.add(new ScreenitDateTextField("geboortedatum").setRequired(true).setOutputMarkupId(true));
		form.add(createSubmitLink());
		add(form);
	}

	private IndicatingAjaxSubmitLink createSubmitLink()
	{
		return new IndicatingAjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				GbaPersoon zoekPersoon = (GbaPersoon) getForm().getModelObject();
				Client client = clientService.getClientByBsn(zoekPersoon.getBsn());
				if (client != null && DateUtil.isGeboortedatumGelijk(DateUtil.toLocalDate(zoekPersoon.getGeboortedatum()), client))
				{
					String melding = kwaliteitscontroleService.addFotobesprekingOnderzoek(getModelObject(), client);
					if (StringUtils.isNotBlank(melding))
					{
						warn(melding);
					}
					else
					{
						onOpslaanSuccesvol(target);
					}
				}
				else
				{
					warn(String.format(getString("error"), zoekPersoon.getBsn(), DateUtil.getGeboortedatum(zoekPersoon), getString("error.null")));
				}
			}
		};
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);
}
