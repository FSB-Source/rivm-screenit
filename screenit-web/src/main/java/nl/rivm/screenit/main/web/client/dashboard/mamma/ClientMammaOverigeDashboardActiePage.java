package nl.rivm.screenit.main.web.client.dashboard.mamma;

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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactAfrondenPopupPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class ClientMammaOverigeDashboardActiePage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(ClientMammaOverigeDashboardActiePage.class);

	private Panel contentPanel;

	@SpringBean
	private ClientContactService clientContactService;

	private final BootstrapDialog dialog;

	private final ClientContactActieType actieType;

	public <T extends AbstractClientContactActiePanel<ClientContactActie>> ClientMammaOverigeDashboardActiePage(IModel<Client> model, Class<T> contentClass,
		final ClientContactActieType actieType)
	{
		this(model, contentClass, actieType, new ArrayList<>());
	}

	public <T extends AbstractClientContactActiePanel<ClientContactActie>> ClientMammaOverigeDashboardActiePage(IModel<Client> model, Class<T> contentClass,
		final ClientContactActieType actieType, List<Object> extraPanelParams)
	{

		this.actieType = actieType;
		setDefaultModel(model);
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		Form<Client> form = new ScreenitForm<Client>("form", model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected Behavior getComponentBorder(Component component)
			{
				return null;
			}

		};
		add(form);

		Object[] initArgs = new Object[] { "content", new Model<ClientContactActie>(), model, extraPanelParams };

		try
		{
			contentPanel = (Panel) ConstructorUtils.invokeConstructor(contentClass, initArgs);
			form.add(contentPanel);
		}
		catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
		{
			LOGGER.error("Fout bij aanmaken panel voor actie " + contentClass.getName(), e);
		}
		Link<Void> annuleren = new Link<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(ClientMammaDashboardPage.class);
			}

		};
		form.add(annuleren);
		annuleren.add(new Label("annulerenTekst", new IModel<Object>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public Object getObject()
			{
				String label = "label.annuleren";
				return getString(label);
			}

		}));

		IndicatingAjaxSubmitLink opslaan = new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				@SuppressWarnings("unchecked")
				final T contentPanelConcreet = (T) contentPanel;
				contentPanelConcreet.validate();

				if (!form.hasError())
				{
					List<String> meldingen = new ArrayList<>();
					meldingen.addAll(contentPanelConcreet.getOpslaanMeldingen());

					if (meldingen.size() > 0)
					{
						dialog.openWith(target, new ClientContactAfrondenPopupPanel(IDialog.CONTENT_ID, meldingen)
						{

							private static final long serialVersionUID = 1L;

							@Override
							protected void opslaan(AjaxRequestTarget target)
							{
								contactAfronden(actieType, contentPanelConcreet);
							}
						});
					}
					else
					{
						contactAfronden(actieType, contentPanelConcreet);
					}
				}
			}

			private <T extends AbstractClientContactActiePanel<ClientContactActie>> void contactAfronden(final ClientContactActieType actieType, T contentPanelConcreet)
			{
				Map<ExtraOpslaanKey, Object> opslaanObjecten = contentPanelConcreet.getOpslaanObjecten();
				ClientContact contact = new ClientContact();
				ClientContactActie actie = new ClientContactActie();
				actie.setType(actieType);
				contact.getActies().add(actie);
				contact.setClient(getIngelogdeClient());

				Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten = new HashMap<>();
				extraOpslaanObjecten.put(actieType, opslaanObjecten);
				Account loggedInAccount = ScreenitSession.get().getLoggedInAccount();
				clientContactService.saveClientContact(contact, extraOpslaanObjecten, loggedInAccount);

				BasePage.markeerFormulierenOpgeslagen(RequestCycle.get().find(AjaxRequestTarget.class).get());

				String label = "message.gegevensopgeslagen";
				switch (actieType)
				{
				case MAMMA_AFMELDEN:
					MammaAfmelding afmelding = (MammaAfmelding) opslaanObjecten.get(ExtraOpslaanKey.AFMELDING);
					if (afmelding.getType().equals(AfmeldingType.EENMALIG))
					{
						label = "success.eenmalig.afgemeld";
					}
					else
					{
						label = "success.definitief.afgemeld";
					}
					break;
				case MAMMA_HERAANMELDEN:
					label = "success.heraanmelden";
					break;
				case BEZWAAR:
					label = "success.bezwaar";
					break;
				default:
					break;
				}
				ScreenitSession.get().success(getString(label));

				setResponsePage(ClientMammaDashboardPage.class);
			}

		};
		form.add(opslaan);
		opslaan.add(new Label("opslaanTekst", new IModel<Object>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public Object getObject()
			{
				String label = "label.opslaan";
				switch (actieType)
				{
				case MAMMA_AFMELDEN:
					label = "label.bevestigen";
					break;
				case MAMMA_HERAANMELDEN:
					label = "label.button.heraanmelden";
					break;
				default:
					break;
				}
				return getString(label);
			}

		}));
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA;
	}

	@Override
	public IModel<String> getPageName()
	{
		String label = "";
		switch (actieType)
		{
		case MAMMA_AFMELDEN:
			label = getString("label.afmelden");
			break;
		case MAMMA_HERAANMELDEN:
			label = getString("label.heraanmelden");
			break;
		case BEZWAAR:
			label = getString("label.bezwaar");
			break;
		default:
			break;
		}
		return new Model<>(label);
	}

}
