package nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class AfdrukkenControlePanel<MB extends MergedBrieven<?>> extends GenericPanel<MB>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	private IndicatingAjaxSubmitLink controleLink;

	private WebMarkupContainer aantalBrievenContainer;

	private IModel<Integer> aantalBrieven;

	public AfdrukkenControlePanel(String id, IModel<MB> model)
	{
		super(id, model);
		Form<MB> form = new Form<>("form", model);
		add(form);

		aantalBrievenContainer = getTextFieldContainer(form, model.getObject());
		form.add(aantalBrievenContainer);

		controleLink = getControleLink(form);
		add(controleLink);

		IndicatingAjaxLink<Void> sluiten = new IndicatingAjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		};
		add(sluiten);
	}

	protected abstract void close(AjaxRequestTarget target);

	private IndicatingAjaxSubmitLink getControleLink(Form<?> form)
	{
		IndicatingAjaxSubmitLink link = new IndicatingAjaxSubmitLink("controle", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MB mBrieven = (MB) form.getModelObject();
				String meldingInvoering = "ingevoerde aantal: " + aantalBrieven.getObject() + " correcte aantal: " + mBrieven.getAantalBrieven() + ".";
				if (isAantalGelijk(mBrieven))
				{

					mBrieven.setControleerDatum(currentDateSupplier.getDate());
					mBrieven.setControle(true);
					hibernateService.saveOrUpdate(mBrieven);

					String melding = getMelding(mBrieven, "Correcte aantal ingevoerd,") + meldingInvoering;
					logging(Level.INFO, melding);

					WebMarkupContainer container = getTextFieldContainer(form, mBrieven);
					aantalBrievenContainer.replaceWith(container);
					aantalBrievenContainer = container;
					target.add(aantalBrievenContainer);

					controleLink.setEnabled(false);
					target.add(controleLink);

					ScreenitSession.get().info("Het ingevoerde aantal komt overeen met het aantal brieven.");
				}
				else
				{
					ScreenitSession.get().error("Het ingevoegde aantal komt niet overeen met het aantal brieven.");
					String melding = getMelding(mBrieven, "Incorrecte aantal ingevoerd,") + meldingInvoering;
					logging(Level.ERROR, melding);
				}
			}
		};
		link.setOutputMarkupId(true);
		add(link);
		return link;
	}

	private WebMarkupContainer getTextFieldContainer(Form<?> form, MB mBrieven)
	{
		WebMarkupContainer container = new WebMarkupContainer("aantalBrievenContainer");

		aantalBrieven = Model.of();
		if (mBrieven.getControle())
		{
			aantalBrieven = Model.of(mBrieven.getAantalBrieven());
		}

		TextField<Integer> aantalBrievenInput = new TextField<>("aantalBrieven", aantalBrieven);
		aantalBrievenInput.setType(Integer.class);
		aantalBrievenInput.setEnabled(!mBrieven.getControle());
		container.add(aantalBrievenInput);
		container.setOutputMarkupId(true);
		aantalBrievenInput.setRequired(true);

		return container;
	}

	private void logging(Level level, String melding)
	{
		Account account = ScreenitSession.get().getLoggedInAccount();

		LogEvent event = new LogEvent(melding);
		event.setLevel(level);

		logService.logGebeurtenis(LogGebeurtenis.BRIEVEN_CONTROLE, event, account, null);
	}

	private boolean isAantalGelijk(MB mBrieven)
	{
		Integer aantal = Integer.valueOf(mBrieven.getAantalBrieven());
		Integer ingevoerdeAantal = Integer.valueOf(aantalBrieven.getObject());
		return aantal.equals(ingevoerdeAantal);
	}

	private String getMelding(MB mBrieven, String melding)
	{
		SimpleDateFormat format = new SimpleDateFormat("dd MMMM yyyy HH:mm:ss");
		if (mBrieven.getMergedBrieven() != null)
		{
			if (mBrieven.getMergedBrieven().getNaam() != null)
			{
				melding += ", document " + mBrieven.getMergedBrieven().getNaam();
			}
			if (mBrieven.getCreatieDatum() != null)
			{
				melding += ", gecre&euml;erd op " + format.format(mBrieven.getCreatieDatum()) + ",  ";
			}
		}
		return melding;
	}
}
