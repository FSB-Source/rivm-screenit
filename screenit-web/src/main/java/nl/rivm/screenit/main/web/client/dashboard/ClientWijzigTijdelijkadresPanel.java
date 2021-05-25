
package nl.rivm.screenit.main.web.client.dashboard;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.PatternValidator;

public class ClientWijzigTijdelijkadresPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private LogService logService;

	public ClientWijzigTijdelijkadresPanel(String id, IModel<Client> model, ClientHoofdMenuitem menu)
	{
		super(id, new CompoundPropertyModel<>(model));

		Client client = model.getObject();
		TijdelijkAdres tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			tijdelijkAdres = new TijdelijkAdres();
			client.getPersoon().setTijdelijkAdres(tijdelijkAdres);
		}
		if (tijdelijkAdres.getStartDatum() == null)
		{
			tijdelijkAdres.setStartDatum(new Date());
		}
		Form<Client> form = new Form<Client>("form", getModel());
		add(form);

		boolean inzien = false;
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.straat", false, 43, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.huisnummer", false, 10, Integer.class, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.huisletter", false, 1, inzien).add(new PatternValidator(".*[a-zA-Z].*"));
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.huisnummerToevoeging", false, 26, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.huisnummerAanduiding", false, 2, inzien);
		ComponentHelper.newPostcodeTextField(form, "persoon.tijdelijkAdres.postcode", false, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.plaats", false, 200, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.startDatum", false, 10, Date.class, inzien);
		ComponentHelper.addTextField(form, "persoon.tijdelijkAdres.eindDatum", false, 10, Date.class, inzien);

		form.add(new ClientTooltipPanel("tijdelijkAdresAanduidingHuisnummerTooltip", ClientTooltipType.TIJDELIJK_ADRES_AANDUIDING_BIJ_HUISNUMMER, false));

		form.add(new Link<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(menu.getTargetClass());
			}

		});

		form.add(new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				GbaPersoon persoon = getModelObject().getPersoon();
				TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
				boolean adresNietVolledig = false;
				boolean adresNietAanmaken = false;
				if (tijdelijkAdres.getId() == null)
				{
					adresNietAanmaken = tijdelijkAdres.getHuisnummer() == null && StringUtils.isBlank(tijdelijkAdres.getHuisnummerToevoeging())
						&& StringUtils.isBlank(tijdelijkAdres.getPlaats()) && StringUtils.isBlank(tijdelijkAdres.getPostcode()) && StringUtils.isBlank(tijdelijkAdres.getStraat())
						&& tijdelijkAdres.getStartDatum() == null;

					if (adresNietAanmaken)
					{
						persoon.setTijdelijkAdres(null);
					}
				}
				if (!adresNietAanmaken)
				{
					adresNietVolledig = tijdelijkAdres.getHuisnummer() == null || StringUtils.isBlank(tijdelijkAdres.getPlaats())
						|| StringUtils.isBlank(tijdelijkAdres.getPostcode()) || StringUtils.isBlank(tijdelijkAdres.getStraat()) 
						|| tijdelijkAdres.getStartDatum() == null;

				}
				if (adresNietVolledig || adresNietAanmaken)
				{
					ScreenitSession.get().error(getString("message.clientgegevenstijdelijkadresnietvolledig"));
				}
				else if (tijdelijkAdres.getEindDatum() != null && !DateUtil.compareAfter(tijdelijkAdres.getEindDatum(), tijdelijkAdres.getStartDatum()))
				{
					ScreenitSession.get().error(getString("error.eindatum.voor.begindatum"));
				}
				else
				{
					clientService.saveOrUpdateClient(getModelObject());
					ScreenitSession.get().success(getString("success.tijdelijkadres"));
					logService.logGebeurtenis(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES, getModelObject());
					setResponsePage(menu.getTargetClass());
				}
			}
		});
	}

}
