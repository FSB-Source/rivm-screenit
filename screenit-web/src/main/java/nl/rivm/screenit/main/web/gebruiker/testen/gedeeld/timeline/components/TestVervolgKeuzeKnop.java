package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestCervixVervolgKeuzeAction;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestMammaVervolgKeuzeAction;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeAction;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.TestVervolgKeuzePopupBasePanel;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.cervix.CervixBaseTestTimelineService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;

import org.apache.commons.beanutils.ConstructorUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class TestVervolgKeuzeKnop extends GenericPanel<List<Client>>
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(TestVervolgKeuzeKnop.class);

	@SpringBean
	protected CervixTestTimelineService cervixTestTimelineService;

	@SpringBean
	protected CervixBaseTestTimelineService cervixBaseTestTimelineService;

	@SpringBean
	protected MammaTestTimelineService mammaTestTimelineService;

	@SpringBean
	protected MammaBaseTestTimelineService mammaBaseTestTimelineService;

	private BootstrapDialog dialog;

	private List<TestVervolgKeuzeOptie> options = null;

	public TestVervolgKeuzeKnop(String id, IModel<List<Client>> model, BootstrapDialog dialog)
	{
		super(id, model);
		this.dialog = dialog;
		add(new AttributeAppender("class", Model.of(" Mogelijke opties")));
		List<TestVervolgKeuzeOptie> opties = getOptions();
		boolean snelkeuzeVisible = !opties.isEmpty();
		boolean snelkeuzes = opties.size() > 1;

		WebMarkupContainer caretContainer = new WebMarkupContainer("caret");
		Component button;
		if (!snelkeuzes && snelkeuzeVisible)
		{
			IndicatingAjaxLink<Void> link = new IndicatingAjaxLink<Void>("button")
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					verwerkKeuze(target, TestVervolgKeuzeKnop.this.options().get(0));
				}
			};
			add(link);
			link.setVisible(isVisible());
			link.add(new Label("buttonlabel", Model.of(opties.get(0).getNaam())));
			caretContainer.setVisible(false);
			link.add(caretContainer);
			button = link;
		}
		else
		{
			WebMarkupContainer buttonContainer = new WebMarkupContainer("button");
			buttonContainer.add(new AttributeAppender("data-toggle", Model.of("dropdown")));
			if ("snelKeuzeMamma".equals(id))
			{
				buttonContainer.add(new Label("buttonlabel", Model.of("Snelkeuze(s) in deze ronde")));
			}
			else
			{
				buttonContainer.add(new Label("buttonlabel", Model.of("Snelkeuze(s)")));
			}
			buttonContainer.setVisible(isVisible());
			caretContainer.setVisible(true);
			buttonContainer.add(caretContainer);
			button = buttonContainer;
		}
		if (StringUtils.isNotBlank(getNameAttribuut()))
		{
			button.add(new AttributeAppender("name", getNameAttribuut()));
		}
		add(button);

		WebMarkupContainer container = new WebMarkupContainer("dropdownList");
		container.setVisible(isVisible());
		add(container);

		ListView<TestVervolgKeuzeOptie> acties = new ListView<>("snelkeuze", opties)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<TestVervolgKeuzeOptie> item)
			{
				final TestVervolgKeuzeOptie option = item.getModelObject();
				IndicatingAjaxLink<TestVervolgKeuzeOptie> link = new IndicatingAjaxLink<>("link")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						verwerkKeuze(target, option);
					}

				};
				link.add(new Label("label", option.getNaam()).add(new AttributeAppender("name", option.toString().toLowerCase().replace(" ", "_"))));
				item.add(link);
			}
		};
		container.add(acties);
	}

	private void verwerkKeuze(AjaxRequestTarget target, TestVervolgKeuzeOptie optie)
	{
		if (TestVervolgKeuzeAction.class.isAssignableFrom(optie.getDetailClass()))
		{
			List<Object> params = new ArrayList<>();
			List<Client> clienten = TestVervolgKeuzeKnop.this.getModel().getObject();
			params.add(clienten);
			if (TestCervixVervolgKeuzeAction.class.isAssignableFrom(optie.getDetailClass()))
			{
				params.add(cervixTestTimelineService);
				params.add(cervixBaseTestTimelineService);
			}
			else if (TestMammaVervolgKeuzeAction.class.isAssignableFrom(optie.getDetailClass()))
			{
				params.add(mammaTestTimelineService);
				params.add(mammaBaseTestTimelineService);
				MammaTestTimelinePage page = (MammaTestTimelinePage) getPage();
				params.add(page.getVerstuurHl7Berichten());
			}
			try
			{
				TestVervolgKeuzeAction action = (TestVervolgKeuzeAction) ConstructorUtils.invokeConstructor(optie.getDetailClass(), params.toArray());
				action.execute();
				refreshContainer(target);
			}
			catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
			{
				LOG.error("Fout bij maken van gebeurtenis detailpanel", e);
			}
		}
		else
		{
			openDialog(target, optie);
		}
	}

	private void openDialog(AjaxRequestTarget target, TestVervolgKeuzeOptie optie)
	{
		dialog.openWith(target, new TestVervolgKeuzePopupBasePanel(IDialog.CONTENT_ID, Model.of(optie), getModel())
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void refreshContainer(AjaxRequestTarget target)
			{
				dialog.close(target);
				TestVervolgKeuzeKnop.this.refreshContainer(target);
			}

		});
	}

	@Override
	public boolean isVisible()
	{
		return true;
	}

	public List<TestVervolgKeuzeOptie> options()
	{
		if (options == null)
		{
			options = getOptions();
		}
		return options;
	}

	public abstract boolean refreshContainer(AjaxRequestTarget target);

	public abstract List<TestVervolgKeuzeOptie> getOptions();

	public abstract String getNameAttribuut();
}
